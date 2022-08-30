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

unit dxEMF.DB.Oracle;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, Generics.Collections, DB, Rtti, dxCore,
  dxEMF.Utils,
  dxEMF.DB.Utils,
  dxEMF.DB.Model,
  dxEMF.Types,
  dxEMF.DB.Query,
  dxEMF.DB.Criteria,
  dxEMF.DB.SQLGenerator,
  dxEMF.DB.SQLConnectionProvider;

type
  { TdxOracleFormatterHelper }

  TdxOracleFormatterHelper = class
  strict private
    const
      StopChars: array [0 .. 1] of Char = ('_', '%');
  public
    class function FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand: string; const ARightOperand: string): string; static;
    class function FormatConstraint(const AConstraintName: string): string; static;
    class function FormatDelete(const ATableName: string; const AWhereClause: string): string; static;
    class function FormatFunction(AOperatorType: TdxFunctionOperatorType; AOperands: TArray<string>): string; overload; static;
    class function FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType; AOperands: TArray<TValue>): string; overload; static;
    class function FormatInsert(const ATableName: string; const AFields: string; const AValues: string): string; static;
    class function FormatInsertDefaultValues(const ATableName: string): string; static;
    class function FormatOrder(const ASortProperty: string; ASortDirection: TdxSortDirection): string; static;
  end;

  { TdxIdentityInsertSqlGenerator }

  TdxIdentityInsertSqlGenerator = class(TdxBaseObjectSqlGenerator)
  strict private
    FSeqName: string;
    FIdentityParameterName: string;
  protected
    function InternalGenerateSQL: string; override;
  public
    constructor Create(const AFormatter: TdxSQLConnectionProvider; const ASeqName, AIdentityParameterName: string; AIdentitiesByTag: TdxTaggedParameterHolder); reintroduce;
  end;

  { TdxBaseOracleConnectionProvider }

  TdxBaseOracleConnectionProvider = class abstract(TdxSQLConnectionProvider)
  public const
    MaximumStringSize = 2000;
    MaximumBinarySize = 2000;
  private
    FCaseSensitiveFieldNames: Boolean;
    FSysUsersAvailable: Boolean;
    FObjectsOwner: string;

    function CheckSysUsers: Boolean;
    function GetCurrentUser: string;
  protected
    function GetOperandArray(AValues: TArray<string>): TArray<IdxOperandValue>;
    function GetDataSet(const ACommandText: string; const AValues, AParameterNames: TArray<string>): TDataSet; overload;
    function GetDataSet(const ACommandText: string): TDataSet; overload;

    function ConvertToDBParameter(const AParameterValue: TValue): TValue; override;
    procedure PrepareDelegates; virtual;
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

    property CaseSensitiveFieldNames: Boolean read FCaseSensitiveFieldNames;
    property SysUsersAvailable: Boolean read FSysUsersAvailable;
    property ObjectsOwner: string read FObjectsOwner;
  public
    constructor Create(AConnectionVendor: TdxCustomConnectionVendor; AAutoCreateOption: TdxAutoCreateOption); override;
  end;

  { TdxOracleConnectionProvider }

  TdxOracleConnectionProvider = class(TdxBaseOracleConnectionProvider)
  private
    FGetForeignKeysHasNoRights: Boolean;

    procedure ClearAndFree(AList: TList<TDataSet>);
    function GetDataForTables(const ATables: TArray<TdxDBTable>; AFilter: TdxOracleConnectionProvider.TTableFilter; const AQueryText: string): TList<TDataSet>;
    procedure GetColumns(ATable: TdxDBTable);
    procedure GetForeignKeys(ATable: TdxDBTable);
    procedure GetIndexes(ATable: TdxDBTable);
    procedure GetPrimaryKey(ATable: TdxDBTable);
    function GetSeqNameCore(ATable: TdxDBTable): string;
    function GetTypeFromString(const ATypeName: string; ASize: Integer; APrecision, AScale: Integer): TdxDBColumnType;
  protected
    function CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>; override;
    function GetMaximumStringSize: Integer; override;
    function GetSafeNameTableMaxLength: Integer; override;
    function GetSeqName(ATable: TdxDBTable): string;
    function GetIdentity(ARoot: TdxInsertStatement; AIdentities: TdxTaggedParameterHolder): Int64; override;
  public
    constructor Create(AConnectionVendor: TdxCustomConnectionVendor; AAutoCreateOption: TdxAutoCreateOption); override;

    class function GetDBEngine: TdxDBEngine; override;

    procedure CreateTable(ATable: TdxDBTable); override;
    function GetConnectionParameter(const AConnectionType: TdxConnectionType; AParameter: TdxConnectionParameter): string; override;
    function GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer; var ACreateParameter: Boolean): string; override;
    function GetSQLCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetStorageTableNames(AIncludeViews: Boolean): TArray<string>; override;
    procedure GetTableSchema(ATable: TdxDBTable; ACheckIndexes, ACheckForeignKeys: Boolean); override;
    function FormatColumn(const AColumnName: string): string; override;
    function FormatColumn(const AColumnName: string; const ATableAlias: string): string; override;
    function FormatConstraint(const AConstraintName: string): string; override;
    function FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string; override;
    function FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string; override;
    function FormatTable(const ASchemaName: string; const ATableName: string): string; override;
    function FormatTable(const ASchemaName: string; const ATableName: string; const ATableAlias: string): string; override;
    function FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string; override;
    function FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause: string;
      ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer): string; override;
    function FormatDelete(const ATableName, AWhereClause: string): string; override;
    function FormatOrder(const ASortProperty: string; ASortDirection: TdxSortDirection): string; override;
    function FormatInsertDefaultValues(ATable: TdxDBTable): string; override;
    function FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string; override;
    function FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string; override;
    function SupportsNativeSkipTake: Boolean; override;
  end;

implementation

uses
  TypInfo, Variants,
  dxStringHelper;

{ TdxOracleFormatterHelper }

class function TdxOracleFormatterHelper.FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand: string; const ARightOperand: string): string;
begin
  case AOperatorType of
    TdxBinaryOperatorType.BitwiseAnd:
      Result := Format('bitand(0:s, %1:s)', [ALeftOperand, ARightOperand]);
    TdxBinaryOperatorType.BitwiseOr:
      Result := Format('(%0:s - bitand(0:s, %1:s) + %1:s)', [ALeftOperand, ARightOperand]);
    TdxBinaryOperatorType.BitwiseXor:
      Result := Format('(0:s - bitand(0:s, %1:s) + %1:s - bitand(%0:s, %1:s))', [ALeftOperand, ARightOperand]);
    TdxBinaryOperatorType.Modulo:
      Result := Format('mod(0:s, %1:s)', [ALeftOperand, ARightOperand]);
    else
      Result := TdxBaseFormatterHelper.DefaultFormatBinary(AOperatorType, ALeftOperand, ARightOperand);
  end;
end;

class function TdxOracleFormatterHelper.FormatConstraint(const AConstraintName: string): string;
begin
  Result := Format('"%s"', [AConstraintName]);
end;

class function TdxOracleFormatterHelper.FormatDelete(const ATableName: string; const AWhereClause: string): string;
begin
  Result := Format('delete from %s where %s', [ATableName, AWhereClause]);
end;

class function TdxOracleFormatterHelper.FormatFunction(AOperatorType: TdxFunctionOperatorType; AOperands: TArray<string>): string;
begin
  case AOperatorType of
    TdxFunctionOperatorType.MillisecondOf:
      raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.SecondOf:
      Result := Format('TO_NUMBER(TO_CHAR(%s, '#$27'SS'#$27'))', [AOperands[0]]);
    TdxFunctionOperatorType.MinuteOf:
      Result := Format('TO_NUMBER(TO_CHAR(%s, '#$27'MI'#$27'))', [AOperands[0]]);
    TdxFunctionOperatorType.HourOf:
      Result := Format('TO_NUMBER(TO_CHAR(%s, '#$27'HH24'#$27'))', [AOperands[0]]);
    TdxFunctionOperatorType.DayOf:
      Result := Format('EXTRACT(DAY FROM (%s))', [AOperands[0]]);
    TdxFunctionOperatorType.MonthOf:
      Result := Format('EXTRACT(MONTH FROM (%s))', [AOperands[0]]);
    TdxFunctionOperatorType.YearOf:
      Result := Format('EXTRACT(YEAR FROM (%s))', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheWeek:
      Result := Format('MOD((TO_NUMBER(TO_CHAR(%s, '#$27'D'#$27'))) - (TO_NUMBER(TO_CHAR(TO_DATE('#$27'01-01-1900'#$27','#$27'DD-MM-YYYY'#$27'), '#$27'D'#$27'))) + 8, 7)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheYear:
      Result := Format('TO_NUMBER(TO_CHAR(%s, '#$27'DDD'#$27'))', [AOperands[0]]);
    TdxFunctionOperatorType.GetTimeOfDay:
      Result := Format('(TO_NUMBER(TO_CHAR(%s, '#$27'SSSSS'#$27')) * 10000000)', [AOperands[0]]);
    TdxFunctionOperatorType.DateOf:
      Result := Format('TRUNC(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.IncTick:
      Result := Format('((%0:s) + NUMTODSINTERVAL(TRUNC((%1:s) / 864000000000), '#$27'DAY'#$27') + NUMTODSINTERVAL(MOD(TRUNC((%1:s) / 10000000),86400), '#$27'SECOND'#$27'))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMillisecond:
      Result := Format('((%0:s) + NUMTODSINTERVAL(TRUNC((%1:s) / 1000), '#$27'SECOND'#$27'))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.AddTimeSpan,
    TdxFunctionOperatorType.IncSecond:
      Result := Format('((%0:s) + NUMTODSINTERVAL(%1:s, '#$27'SECOND'#$27'))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMinute:
      Result := Format('((%0:s) + NUMTODSINTERVAL(TRUNC(((%1:s) * 60) / 86400), '#$27'DAY'#$27') + NUMTODSINTERVAL(MOD(TRUNC((%1:s) * 60),86400), '#$27'SECOND'#$27'))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncHour:
      Result := Format('((%0:s) + NUMTODSINTERVAL(TRUNC(((%1:s) * 3600) / 86400), '#$27'DAY'#$27') + NUMTODSINTERVAL(MOD(TRUNC((%1:s) * 3600),86400), '#$27'SECOND'#$27'))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncDay:
      Result := Format('((%0:s) + NUMTODSINTERVAL(TRUNC(%1:s), '#$27'DAY'#$27') + NUMTODSINTERVAL(MOD(TRUNC((%1:s) * 86400),86400), '#$27'SECOND'#$27'))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMonth:
      Result := Format('ADD_MONTHS(%0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncYear:
      Result := Format('ADD_MONTHS(%0:s, (%1:s) * 12)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.YearsBetween:
      Result := Format('(EXTRACT(YEAR FROM (%1:s)) - EXTRACT(YEAR FROM (%0:s)))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MonthsBetween:
      Result := Format('(((EXTRACT(YEAR FROM (%1:s)) - EXTRACT(YEAR FROM (%0:s))) * 12) + EXTRACT(MONTH FROM (%1:s)) - EXTRACT(MONTH FROM (%0:s)))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.DaysBetween:
      Result := Format('(TRUNC(%1:s) - TRUNC(%0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.HoursBetween:
      Result := Format('((TRUNC((%1:s), '#$27'HH24'#$27') - TRUNC((%0:s), '#$27'HH24'#$27')) * 24)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MinutesBetween:
      Result := Format('((TRUNC((%1:s), '#$27'MI'#$27') - TRUNC((%0:s), '#$27'MI'#$27')) * 1440)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.SecondsBetween:
      Result := Format('(ROUND((TRUNC((%1:s), '#$27'MI'#$27') - TRUNC((%0:s), '#$27'MI'#$27')) * 86400) + TO_NUMBER(TO_CHAR((%1:s), '#$27'SS'#$27')) - TO_NUMBER(TO_CHAR((%0:s), '#$27'SS'#$27')))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MillisecondsBetween:
      Result := Format('((ROUND((TRUNC((%1:s), '#$27'MI'#$27') - TRUNC((%0:s), '#$27'MI'#$27')) * 86400) + TO_NUMBER(TO_CHAR((%1:s), '#$27'SS'#$27')) - TO_NUMBER(TO_CHAR((%0:s), '#$27'SS'#$27'))) * 1000)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.DateDiffTicks:
      Result := Format('((ROUND((TRUNC((%1:s), '#$27'MI'#$27') - TRUNC((%0:s), '#$27'MI'#$27')) * 86400) + TO_NUMBER(TO_CHAR((%1:s), '#$27'SS'#$27')) - TO_NUMBER(TO_CHAR((%0:s), '#$27'SS'#$27'))) * 10000000)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Now:
      Result := 'SYSDATE';
    TdxFunctionOperatorType.Today:
      Result := 'TRUNC(SYSDATE)';
    TdxFunctionOperatorType.Abs:
      Result := Format('ABS(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.BigMul:
      Result := Format('CAST(((%0:s) * (%1:s)) as INTEGER)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Sqr:
      Result := Format('SQRT(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Random:
      raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.Log:
      case Length(AOperands) of
        1:
          Result := Format('LN(%0:s)', [AOperands[0]]);
        2:
          Result := Format('Log(%1:s, %0:s)', [AOperands[0], AOperands[1]]);
      else
        Result := '';
      end;
    TdxFunctionOperatorType.Log10:
      Result := Format('Log(10, %0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sin:
      Result := Format('SIN(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Tan:
      Result := Format('TAN(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan:
      Result := Format('ATAN(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan2:
      Result := Format('(case when (%1:s) = 0 then Sign(%0:s) * Atan(1) * 2 else ATAN2(%0:s, %1:s) end)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Cos:
      Result := Format('COS(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcCos:
      Result := Format('ACOS(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcSin:
      Result := Format('ASIN(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Cosh:
      Result := Format('Cosh(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sinh:
      Result := Format('Sinh(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Tanh:
      Result := Format('Tanh(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Exp:
      Result := Format('EXP(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Power:
      Result := Format('POWER(%0:s,%1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Round:
      case Length(AOperands) of
        1:
          Result := Format('ROUND(%0:s,0)', [AOperands[0]]);
        2:
          Result := Format('ROUND(%0:s,%1:s)', [AOperands[0], AOperands[1]]);
      else
        Result := '';
      end;
    TdxFunctionOperatorType.Sign:
      Result := Format('SIGN(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Max:
      Result := Format('(case when %0:s > %1:s then %0:s else %1:s end)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Min:
      Result := Format('(case when %0:s < %1:s then %0:s else %1:s end)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Floor:
      Result := Format('FLOOR(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Ceil:
      Result := Format('CEIL(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.ToInteger:
      Result := Format('TRUNC(CAST((%0:s) AS INT))', [AOperands[0]]);
    TdxFunctionOperatorType.ToInt64:
      Result := Format('TRUNC(CAST((%0:s) AS NUMBER(20,0)))', [AOperands[0]]);
    TdxFunctionOperatorType.ToSingle:
      Result := Format('CAST((%0:s) AS FLOAT)', [AOperands[0]]);
    TdxFunctionOperatorType.ToDouble:
      Result := Format('CAST((%0:s) AS DOUBLE PRECISION)', [AOperands[0]]);
    TdxFunctionOperatorType.ToDecimal:
      Result := Format('CAST((%0:s) AS NUMBER(20,5))', [AOperands[0]]);
    TdxFunctionOperatorType.ToString:
      Result := Format('TO_CHAR(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Ascii:
      Result := Format('ASCII(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Char:
      Result := Format('CHR(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Replace:
      Result := Format('REPLACE(%0:s, %1:s, %2:s)', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Reverse:
      Result := Format('REVERSE(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Insert:
      Result := Format('(SUBSTR(%0:s, 1, %1:s) || (%2:s) || SUBSTR(%0:s, (%1:s) +  1))', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Remove:
      case Length(AOperands) of
        2:
          Result := Format('SUBSTR(%0:s, 1, %1:s)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('(SUBSTR(%0:s, 1, %1:s) || SUBSTR(%0:s, (%1:s) + (%2:s) + 1))', [AOperands[0], AOperands[1], AOperands[2]]);
      else
        Result := '';
      end;
    TdxFunctionOperatorType.IsNull:
      case Length(AOperands) of
        1:
          Result := Format('((%0:s) is null)', [AOperands[0]]);
        2:
          Result := Format('NVL(%0:s, %1:s)', [AOperands[0], AOperands[1]]);
      else
        Result := '';
      end;
    TdxFunctionOperatorType.Substring:
      case Length(AOperands) of
        2:
          Result := Format('SUBSTR(%0:s, (%1:s) + 1)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('SUBSTR(%0:s, (%1:s) + 1, %2:s)', [AOperands[0], AOperands[1], AOperands[2]]);
      else
        Result := '';
      end;
    TdxFunctionOperatorType.Len:
      Result := Format('LENGTH(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.CharIndex:
      case Length(AOperands) of
        2:
          Result := Format('(INSTR(%1:s, %0:s) - 1)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('(INSTR(%1:s, %0:s, (%2:s) + 1) - 1)', [AOperands[0], AOperands[1], AOperands[2]]);
        4:
          Result := Format('(INSTR(SUBSTR(%1:s, 1, (%2:s) + (%3:s)), %0:s, (%2:s) + 1) - 1)', [AOperands[0], AOperands[1], AOperands[2], AOperands[3]]);
      else
        Result := '';
      end;
    TdxFunctionOperatorType.PadLeft:
      case Length(AOperands) of
        2:
          Result := Format('(case when length(%0:s) > (%1:s) then %0:s else lpad(%0:s, %1:s) end)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('(case when length(%0:s) > (%1:s) then %0:s else lpad(%0:s, %1:s, %2:s) end)', [AOperands[0], AOperands[1], AOperands[2]]);
      else
        Result := '';
      end;
    TdxFunctionOperatorType.PadRight:
      case Length(AOperands) of
        2:
          Result := Format('(case when length(%0:s) > %1:s then %0:s else rpad(%0:s, %1:s) end)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('(case when length(%0:s) > %1:s then %0:s else rpad(%0:s, %1:s, %2:s) end)', [AOperands[0], AOperands[1], AOperands[2]]);
      else
        Result := '';
      end;
    TdxFunctionOperatorType.IsNullOrEmpty:
      Result := Format('((%0:s) is null or length(%0:s) = 0)', [AOperands[0]]);
    TdxFunctionOperatorType.EndsWith:
      Result := Format('(SUBSTR((%0:s), GREATEST(-LENGTH(%0:s), -LENGTH(%1:s))) = (%1:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Contains:
      Result := Format('(Instr(%0:s, %1:s) > 0)', [AOperands[0], AOperands[1]]);
    else
      Result := '';
  end;
end;

class function TdxOracleFormatterHelper.FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType; AOperands: TArray<TValue>): string;
var
  ASecondOperand: TObject;
  AConstantValue: IdxCriteriaOperator;
  AOperandString: string;
  ALikeIndex: Integer;
begin
  case AOperatorType of
    TdxFunctionOperatorType.StartsWith:
     begin
        ASecondOperand := AOperands[1].AsObject;
        if (ASecondOperand is TdxOperandValue) and TdxOperandValue(ASecondOperand).Value.IsString then
        begin
          AOperandString := TdxOperandValue(ASecondOperand).Value.AsString;
          ALikeIndex := {$IFDEF DELPHIXE3}AOperandString.IndexOfAny(StopChars){$ELSE}TdxStringHelper.IndexOfAny(AOperandString, StopChars){$ENDIF};
          if ALikeIndex < 0 then
          begin
            AConstantValue := TdxConstantValue.Create(AOperandString + '%');
            Exit(Format('(%0:s like %1:s)', [AProcessParameter(Safe<TdxCriteriaOperator>.Cast(AOperands[0].AsObject)), AProcessParameter(AConstantValue)]))
          end
          else
            if ALikeIndex > 0 then
              Exit(Format('((%0:s like %2:s) And (Instr(%0:s, %1:s) = 1))',
                [AProcessParameter(Safe<TdxCriteriaOperator>.Cast(AOperands[0].AsObject)),
                AProcessParameter(Safe<TdxCriteriaOperator>.Cast(ASecondOperand)),
                AProcessParameter(TdxConstantValue.Create(
                  {$IFDEF DELPHIXE3}AOperandString.Substring(0, ALikeIndex){$ELSE}TdxStringHelper.Substring(AOperandString, 0, ALikeIndex){$ENDIF} + '%'))]));
        end;
        Result := Format('(Instr(%0:s, %1:s) = 1)', [AProcessParameter(Safe<TdxCriteriaOperator>.Cast(AOperands[0].AsObject)), AProcessParameter(Safe<TdxCriteriaOperator>.Cast(ASecondOperand))]);
     end;
    else
      Result := '';
  end;
end;

class function TdxOracleFormatterHelper.FormatInsert(const ATableName: string; const AFields: string; const AValues: string): string;
begin
  Result := Format('insert into %0:s(%1:s)values(%2:s)', [ATableName, AFields, AValues]);
end;

class function TdxOracleFormatterHelper.FormatInsertDefaultValues(const ATableName: string): string;
begin
  Result := Format('insert into %s values(default)', [ATableName]);
end;

class function TdxOracleFormatterHelper.FormatOrder(const ASortProperty: string; ASortDirection: TdxSortDirection): string;
const
  SortingMap: array [TdxSortDirection] of string = ('asc nulls first', 'desc nulls last');
begin
  Result := Format('%0:s %1:s', [ASortProperty, SortingMap[ASortDirection]]);
end;

{ TdxIdentityInsertSqlGenerator }

constructor TdxIdentityInsertSqlGenerator.Create(const AFormatter: TdxSQLConnectionProvider; const ASeqName, AIdentityParameterName: string; AIdentitiesByTag: TdxTaggedParameterHolder);
begin
  inherited Create(AFormatter, AIdentitiesByTag);
  FSeqName := ASeqName;
  FIdentityParameterName := AIdentityParameterName;
end;

function TdxIdentityInsertSqlGenerator.InternalGenerateSql: string;
var
  AHasIdentityParameter: Boolean;
  ANames, AValues: TStringBuilder;
  I: Integer;
begin
  AHasIdentityParameter := FIdentityParameterName <> '';
  ANames := TStringBuilder.Create;
  AValues := TStringBuilder.Create;
  try
    for I := 0 to Root.Operands.Count - 1 do
    begin
      ANames.
        Append(Process(Root.Operands[I])).
        Append(',');
      AValues.
        Append(GetNextParameterName((TdxInsertStatement(Root)).Parameters[I])).
        Append(',');
    end;
    ANames.
      Append(FFormatter.FormatColumn(FFormatter.ComposeSafeColumnName((TdxInsertStatement(Root)).IdentityColumn)));
    if AHasIdentityParameter then
      AValues.Append(FIdentityParameterName)
    else
      AValues.Append(FSeqName).Append('.nextval');
    Result := FFormatter.FormatInsert(FFormatter.FormatTable(FFormatter.ComposeSafeSchemaName(Root.Table.Name),
      FFormatter.ComposeSafeTableName(Root.Table.Name)), ANames.ToString, AValues.ToString);
    if AHasIdentityParameter then
      Result := Format('select %s.nextval into %s from DUAL;%s;'#10, [FSeqName, FIdentityParameterName, Result]);
  finally
    AValues.Free;
    ANames.Free;
  end;
end;

type
  TdxConnectionVendorAccess = class(TdxCustomConnectionVendor);

{ TdxBaseOracleConnectionProvider }

constructor TdxBaseOracleConnectionProvider.Create(AConnectionVendor: TdxCustomConnectionVendor; AAutoCreateOption: TdxAutoCreateOption);
begin
  inherited Create(AConnectionVendor, AAutoCreateOption);
  PrepareDelegates;
  FCaseSensitiveFieldNames := TdxConnectionVendorAccess(AConnectionVendor).GetCaseSensitiveFieldNames;
  FObjectsOwner := GetCurrentUser;
  FSysUsersAvailable := CheckSysUsers;
//  ReadDBCharset;
end;

function TdxBaseOracleConnectionProvider.GetOperandArray(AValues: TArray<string>): TArray<IdxOperandValue>;
var
  I: Integer;
  AValue: string;
begin
  SetLength(Result, Length(AValues));
  for I := 0 to Length(AValues) - 1 do
    Result[I] := TdxOperandValue.Create(AValue);
end;

function TdxBaseOracleConnectionProvider.GetDataSet(const ACommandText: string; const AValues, AParameterNames: TArray<string>): TDataSet;
var
  AParameters: TdxQueryParameterCollection;
  AQuery: TdxQuery;
begin
  AParameters := TdxQueryParameterCollection.Create;
  AParameters.AddRange(GetOperandArray(AValues));
  AQuery := TdxQuery.Create(ACommandText, AParameters, AParameterNames);
  try
   Result := SelectData(AQuery);
  finally
    AQuery.Free;
    AParameters.Free;
  end;
end;

function TdxBaseOracleConnectionProvider.GetDataSet(const ACommandText: string): TDataSet;
var
  AQuery: TdxQuery;
begin
  AQuery := TdxQuery.Create(ACommandText);
  try
   Result := SelectData(AQuery);
  finally
    AQuery.Free;
  end;
end;

function TdxBaseOracleConnectionProvider.ConvertToDBParameter(const AParameterValue: TValue): TValue;
const
  ABoolAsByte: array[Boolean] of Byte = (0, 1);
begin
  if AParameterValue.IsType<TGUID> then
    Result := Copy(GUIDToString(AParameterValue.AsType<TGUID>), 2, 36)
  else
    if AParameterValue.IsType<Boolean> then
      Result := TValue.From<Byte>(ABoolAsByte[AParameterValue.AsBoolean])
    else
      Result := inherited ConvertToDBParameter(AParameterValue);
end;

procedure TdxBaseOracleConnectionProvider.PrepareDelegates;
begin
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForBoolean(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'number(1,0)';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'number(3,0)';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForSByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'number(3,0)';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForChar(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'nchar';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForDecimal(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'number(20,5)';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForDouble(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'double precision';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForSingle(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'float';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'int';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForUInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(10,0)';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'number(5,0)';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForUInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'number(5,0)';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'number(20,0)';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForUInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'number(20,0)';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForString(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  if (AColumn.Size > 0) and (AColumn.Size <= MaximumStringSize) then
    Exit(Format('nvarchar2(%d)', [AColumn.Size]))
  else
    if AColumn.Size = 0 then
      Exit(Format('nvarchar2(%d)', [DefaultStringSize]))
    else
      Exit('nclob');
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForDateTime(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'date';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForGuid(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'char(36)';
end;

function TdxBaseOracleConnectionProvider.GetSqlCreateColumnTypeForByteArray(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  if (AColumn.Size > 0) and (AColumn.Size < MaximumBinarySize) then
    Result := 'raw(' + IntToStr(AColumn.Size) + ')'
  else
    Result := 'blob';
end;

function TdxBaseOracleConnectionProvider.CheckSysUsers: Boolean;
var
  AQuery1, AQuery2: TdxQuery;
begin
  AQuery1 := TdxQuery.Create('SELECT count(*) FROM "SYS"."ALL_TABLES" WHERE "OWNER" = ''SYS'' AND "TABLE_NAME" = ''USER$''');
  AQuery2 := TdxQuery.Create('SELECT count(*) FROM "SYS"."USER$"');
  try
    try
      Result := GetScalar(AQuery1) = 1;
      if Result then
        GetScalar(AQuery2);
    except
      Result := False;
    end;
  finally
    AQuery2.Free;
    AQuery1.Free;
  end;
end;

function TdxBaseOracleConnectionProvider.GetCurrentUser: string;
var
  ADataSet: TDataSet;
begin
  ADataSet := GetDataSet('select user CURRENT_USER from DUAL');
  try
    Result := ADataSet.Fields[0].AsString;
  finally
    ADataSet.Free;
  end;
end;

{ TdxOracleConnectionProvider }

function TdxOracleConnectionProvider.CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>;
var
  ADBTables, ADBSchemaTables: TDictionary<string, Boolean>;
  AQueryString, ATableName, ATableSchemaName : string;
  AList: TList<TdxDBTable>;
  ATable: TdxDBTable;
  AIsView: Boolean;
  ADataSets: TList<TDataSet>;
  ADataSet: TDataSet;
begin
  ADBTables := TDictionary<string, Boolean>.Create;
  ADBSchemaTables := TDictionary<string, Boolean>.Create;
  try
    if SysUsersAvailable then
    begin
      AQueryString := 'select o.TABLE_NAME, o.OWNER from SYS.ALL_TABLES o inner join SYS.USER$ u on o."OWNER" = u."NAME"'#13#10 +
        'where u."NAME" <> ''SYS'' and u."NAME" <> ''SYSTEM'' and u."TYPE#" = 1 and u.ASTATUS = 0 and o.TABLE_NAME in (%s)';
      ADataSets := GetDataForTables(ATables, nil, AQueryString);
      try
        for ADataSet in ADataSets do
        begin
          while not ADataSet.Eof do
          begin
            if ADataSet.Fields[0].IsNull then
              Continue;
            ATableName := ADataSet.Fields[0].AsString;
            ATableSchemaName := ADataSet.Fields[1].AsString;
            ADBSchemaTables.Add(Concat(ATableSchemaName, '.', ATableName), False);
            ADataSet.Next;
          end;
        end;
      finally
        ClearAndFree(ADataSets);
      end;

      AQueryString := 'select o.VIEW_NAME, o.OWNER from SYS.ALL_VIEWS o inner join SYS.USER$ u on o."OWNER" = u."NAME"'#13#10 +
        'where u."NAME" <> ''SYS'' and u."NAME" <> ''SYSTEM'' and u."TYPE#" = 1 and u.ASTATUS = 0 and o.VIEW_NAME in (%s)';
      ADataSets := GetDataForTables(ATables, nil, AQueryString);
      try
        for ADataSet in ADataSets do
        begin
          while not ADataSet.Eof do
          begin
            if ADataSet.Fields[0].IsNull then
              Continue;
            ATableName := ADataSet.Fields[0].AsString;
            ATableSchemaName := ADataSet.Fields[1].AsString;
            ADBSchemaTables.Add(Concat(ATableSchemaName, '.', ATableName), True);
            ADataSet.Next;
          end;
        end;
      finally
        ClearAndFree(ADataSets);
      end;
    end;

    AQueryString := 'select TABLE_NAME from USER_TABLES where TABLE_NAME in (%s)';
    ADataSets := GetDataForTables(ATables, nil, AQueryString);
    try
      for ADataSet in ADataSets do
      begin
        while not ADataSet.Eof do
        begin
          ADBTables.AddOrSetValue(ADataSet.Fields[0].AsString, False);
          ADataSet.Next;
        end;
      end;
    finally
      ClearAndFree(ADataSets);
    end;

    AQueryString := 'select VIEW_NAME from USER_VIEWS where VIEW_NAME in (%s)';
    ADataSets := GetDataForTables(ATables, nil, AQueryString);
    try
      for ADataSet in ADataSets do
      begin
        while not ADataSet.Eof do
        begin
          ADBTables.AddOrSetValue(ADataSet.Fields[0].AsString, True);
          ADataSet.Next;
        end;
      end;
    finally
      ClearAndFree(ADataSets);
    end;

    AList := TList<TdxDBTable>.Create;
    try
      for ATable in ATables do
      begin
        ATableName := ComposeSafeTableName(ATable.Name);
        ATableSchemaName := ComposeSafeSchemaName(ATable.Name);
        AIsView := False;
        if not ADBSchemaTables.TryGetValue(Concat(ATableSchemaName, '.', ATableName), AIsView) and not ADBTables.TryGetValue(ATableName, AIsView) then
          AList.Add(ATable)
        else
          ATable.IsView := AIsView;
      end;
      Result := AList.ToArray;
    finally
      AList.Free;
    end;
  finally
    ADBSchemaTables.Free;
    ADBTables.Free;
  end;
end;

function TdxOracleConnectionProvider.GetIdentity(ARoot: TdxInsertStatement; AIdentities: TdxTaggedParameterHolder): Int64;
var
  ASeq: string;
  AQuery: TdxQuery;
  AGenerator: TdxIdentityInsertSqlGenerator;
begin
  ASeq := GetSeqName(ARoot.Table);
  AGenerator := TdxIdentityInsertSqlGenerator.Create(Self, ASeq, '', AIdentities);
  try
    AQuery := AGenerator.GenerateSql(ARoot);
    try
      Execute(AQuery);
    finally
      AQuery.Free;
    end;
    AQuery := TdxQuery.Create(Format('select %s.currval from DUAL', [ASeq]));
    try
      Result := GetScalar(AQuery);
    finally
      AQuery.Free;
    end;
  finally
    AGenerator.Free;
  end;
end;

function TdxOracleConnectionProvider.GetMaximumStringSize: Integer;
begin
  Result := TdxBaseOracleConnectionProvider.MaximumStringSize;
end;

function TdxOracleConnectionProvider.GetSafeNameTableMaxLength: Integer;
begin
  Result := 30;
end;

constructor TdxOracleConnectionProvider.Create(AConnectionVendor: TdxCustomConnectionVendor; AAutoCreateOption: TdxAutoCreateOption);
begin
  inherited;
  FSysUsersAvailable := CheckSysUsers;
end;

class function TdxOracleConnectionProvider.GetDBEngine: TdxDBEngine;
begin
  Result := TdxDBEngines.Oracle;
end;

procedure TdxOracleConnectionProvider.CreateTable(ATable: TdxDBTable);
var
  AKey: TdxDBColumn;
  AQuery: TdxQuery;
  ASequenceName: string;
  N: Integer;
begin
  inherited CreateTable(ATable);
  if ATable.PrimaryKey <> nil then
  begin
    AKey := ATable.GetColumn(ATable.PrimaryKey.Columns[0]);
    if AKey.IsIdentity then
    begin
      ASequenceName := GetSeqNameCore(ATable);
      if SysUsersAvailable then
        AQuery := TdxQuery.Create('select count(*) from SYS.ALL_SEQUENCES o inner join SYS.USER$ u on o."SEQUENCE_OWNER" = u."NAME"'#13#10 +
        'where u."NAME" <> '#$27'SYS'#$27' and u."NAME" <> '#$27'SYSTEM'#$27' and u."TYPE#" = 1 and u.ASTATUS = 0 and '+
        'o.SEQUENCE_NAME = ''' + ASequenceName + '''')
      else
        AQuery := TdxQuery.Create('select count(*) from USER_SEQUENCES where SEQUENCE_NAME = ''' + ASequenceName + '''');
      try
        N := GetScalar(AQuery);
        if N <> 0 then
          Exit;
      finally
        AQuery.Free;
      end;
      AQuery := TdxQuery.Create(Format('create sequence %s START WITH 1 INCREMENT BY 1', [GetSeqName(ATable)]));
      try
        Execute(AQuery);
      finally
        AQuery.Free;
      end;
    end;
  end;
end;

function TdxOracleConnectionProvider.GetConnectionParameter(const AConnectionType: TdxConnectionType; AParameter: TdxConnectionParameter): string;
begin
  raise Exception.Create('Error GetConnectionParameter');
end;

function TdxOracleConnectionProvider.GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer;
  var ACreateParameter: Boolean): string;
var
  AValue: TValue;
begin
  AValue := AParameter.Value;
  ACreateParameter := False;
  if (AParameter is TdxConstantValue) and not AValue.IsEmpty then
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
          Exit(IntToStr(AParameter.Value.AsInteger));
        tkString, tkLString, tkWString, tkUString:
          Exit('N'#$27 + ReplaceText(AParameter.Value.AsString, #$27, #$27#$27) + #$27);
    end;
  end;
  ACreateParameter := True;
  Result := GetParamAlias(AIndex);
end;

function TdxOracleConnectionProvider.GetSQLCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  if AColumn.IsKey and AColumn.IsIdentity and (AColumn.ColumnType in [TdxDBColumnType.Int32, TdxDBColumnType.Int64]) and
    IsSingleColumnPKColumn(ATable, AColumn) then
    Result := 'number PRIMARY KEY'
  else
    Result := GetSqlCreateColumnType(ATable, AColumn);
  if not AColumn.IsKey and (AColumn.IsNullable or (AColumn.ColumnType in [TdxDBColumnType.String, TdxDBColumnType.Guid])) then
    Result := Result + ' NULL'
  else
    Result := Result + ' NOT NULL';
end;

function TdxOracleConnectionProvider.GetStorageTableNames(AIncludeViews: Boolean): TArray<string>;
var
  AQueryStrings: TArray<string>;
  ADataSets: TList<TDataSet>;
  AQueryString, AObjectName, AOwner: string;
  ADataSet: TDataSet;
  AResultList: TList<string>;
begin
  if SysUsersAvailable then
    AQueryStrings := TArray<string>.Create(
      'select o.TABLE_NAME, o.OWNER from SYS.All_TABLES o inner join SYS.USER$ u on o."OWNER" = u."NAME"' +
        'where u."NAME" <> ''SYS'' and u."NAME" <> ''SYSTEM'' and u."TYPE#" = 1 and u.ASTATUS = 0',
      'select o.VIEW_NAME, o.OWNER from SYS.All_VIEWS o inner join SYS.USER$ u on o."OWNER" = u."NAME"' +
        'where u."NAME" <> ''SYS'' and u."NAME" <> ''SYSTEM'' and u."TYPE#" = 1 and u.ASTATUS = 0')
  else
    AQueryStrings := TArray<string>.Create(
      'select TABLE_NAME from USER_TABLES',
      'select VIEW_NAME from USER_VIEWS');

  ADataSets := TList<TDataSet>.Create;
  try
    for AQueryString in AQueryStrings do
    begin
      ADataSet := GetDataSet(AQueryString);
      ADataSets.Add(ADataSet);
      if not AIncludeViews then
        Break;
    end;
    AResultList := TList<string>.Create;
    try
      for ADataSet in ADataSets do
      begin
        while not ADataSet.Eof do
        begin
          if Pos('BIN$', ADataSet.Fields[0].AsString) <> 1 then
          begin
            AObjectName := ADataSet.Fields[0].AsString;
            if SysUsersAvailable then
            begin
              AOwner := ADataSet.Fields[1].AsString;
              if (ObjectsOwner <> AOwner) and (AOwner <> '') then
                AResultList.Add(AOwner + '.' + AObjectName)
              else
                AResultList.Add(AObjectName);
            end
            else
              AResultList.Add(AObjectName);
          end;
          ADataSet.Next;
        end;
      end;
      Result := AResultList.ToArray;
    finally
      AResultList.Free;
    end;
  finally
    ClearAndFree(ADataSets);
  end;
end;

procedure TdxOracleConnectionProvider.GetTableSchema(ATable: TdxDBTable; ACheckIndexes, ACheckForeignKeys: Boolean);
begin
  GetColumns(ATable);
  GetPrimaryKey(ATable);
  if ACheckIndexes then
    GetIndexes(ATable);
  if ACheckForeignKeys then
    GetForeignKeys(ATable);
end;

function TdxOracleConnectionProvider.FormatColumn(const AColumnName: string): string;
begin
  if CaseSensitiveFieldNames then
    Result := Format('"%s"', [UpperCase(AColumnName)])
  else
    Result := Format('"%s"', [AColumnName]);
end;

function TdxOracleConnectionProvider.FormatColumn(const AColumnName: string; const ATableAlias: string): string;
begin
  Result := ATableAlias + '.' + FormatColumn(AColumnName);
end;

function TdxOracleConnectionProvider.FormatConstraint(const AConstraintName: string): string;
begin
  Result := TdxOracleFormatterHelper.FormatConstraint(AConstraintName);
end;

function TdxOracleConnectionProvider.FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string;
begin
  Result := TdxOracleFormatterHelper.FormatFunction(AOperatorType, AOperands);
  if Result = '' then
    Result := inherited FormatFunction(AOperatorType, AOperands);
end;

function TdxOracleConnectionProvider.FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string;
begin
  Result := TdxOracleFormatterHelper.FormatFunction(AProcessParameter, AOperatorType, AOperands);
  if Result = '' then
    Result := inherited FormatFunction(AProcessParameter, AOperatorType, AOperands);
end;

function TdxOracleConnectionProvider.FormatTable(const ASchemaName: string; const ATableName: string): string;
begin
  if ASchemaName = '' then
    Result := Format('"%s"', [ATableName])
  else
    Result := Format('"%s"."%s"', [ASchemaName, ATableName]);
end;

function TdxOracleConnectionProvider.FormatTable(const ASchemaName: string; const ATableName: string; const ATableAlias: string): string;
begin
  if ASchemaName = '' then
    Result := Format('"%0:s" %1:s', [ATableName, ATableAlias])
  else
    Result := Format('"%0:s"."%1:s" %2:s', [ASchemaName, ATableName, ATableAlias]);
end;

function TdxOracleConnectionProvider.FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string;
begin
  Result := TdxOracleFormatterHelper.FormatBinary(AOperatorType, ALeftOperand, ARightOperand);
end;

function TdxOracleConnectionProvider.FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause: string;
  ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer): string;
var
  AExpandedWhereSql, AExpandedOrderBySql, AExpandedHavingSql, AExpandedGroupBySql, ASelectedSql, ABaseFormat: string;
  AFields: TArray<string>;
  AExpandedSelectedProperties: string;
begin
  inherited FormatSelect(ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause, ASkipSelectedRecords, ATopSelectedRecords);
  if AWhereClause = '' then
    AExpandedWhereSql := ''
  else
    AExpandedWhereSql := #10'where ' + AWhereClause;
  if AOrderByClause <> '' then
    AExpandedOrderBySql := #10'order by ' + AOrderByClause
  else
    AExpandedOrderBySql := '';
  if AHavingClause <> '' then
    AExpandedHavingSql := #10'having ' + AHavingClause
  else
    AExpandedHavingSql := '';
  if AGroupByClause <> '' then
    AExpandedGroupBySql := #10'group by ' + AGroupByClause
  else
    AExpandedGroupBySql := '';
  AFields := TdxSimpleSQLParser.GetColumns(ASelectClause);
  AExpandedSelectedProperties := TdxSimpleSQLParser.GetExpandedProperties(AFields, 'a');
  ASelectedSql := TdxFormatterHelper.Concat(AFields, ', ');
  if ASkipSelectedRecords = 0 then
  begin
    if ATopSelectedRecords = 0 then
      Result := Format('select %0:s from %1:s%2:s%3:s%4:s%5:s',
        [ASelectClause, AFromClause, AExpandedWhereSql, AExpandedGroupBySql, AExpandedHavingSql, AExpandedOrderBySql])
    else
      Result := Format('select * from (select %1:s from %2:s%3:s%4:s%5:s%6:s) where RowNum <= %0:d',
        [ATopSelectedRecords, ASelectedSql, AFromClause, AExpandedWhereSql, AExpandedGroupBySql, AExpandedHavingSql, AExpandedOrderBySql]);
  end
  else
  begin
    ABaseFormat := 'select %0:s from(select %0:s, RowNum rNum from(select %1:s from %2:s%3:s%4:s%5:s%6:s)a)a where rNum > %7:d';
    if ATopSelectedRecords <> 0 then
      ABaseFormat := ABaseFormat + ' and RowNum <= %8:d';
    Result := Format(ABaseFormat, [AExpandedSelectedProperties, ASelectedSql, AFromClause, AExpandedWhereSql, AExpandedGroupBySql, AExpandedHavingSql, AExpandedOrderBySql, ASkipSelectedRecords, ATopSelectedRecords]);
  end;
end;

function TdxOracleConnectionProvider.FormatDelete(const ATableName, AWhereClause: string): string;
begin
  Result := TdxOracleFormatterHelper.FormatDelete(ATableName, AWhereClause);
end;

function TdxOracleConnectionProvider.FormatOrder(const ASortProperty: string; ASortDirection: TdxSortDirection): string;
begin
  Result := TdxOracleFormatterHelper.FormatOrder(ASortProperty, ASortDirection);
end;

function TdxOracleConnectionProvider.FormatInsertDefaultValues(ATable: TdxDBTable): string;
begin
  Result := TdxOracleFormatterHelper.FormatInsertDefaultValues(FormatTableSafe(ATable));
end;

function TdxOracleConnectionProvider.FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string;
begin
  Result := TdxOracleFormatterHelper.FormatInsert(ATableName, AFieldNames, AValuesClause);
end;

function TdxOracleConnectionProvider.FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string;
begin
  Result := Format('update %s set %s where %s', [ATableName, ASetClause, AWhereClause]);
end;

function TdxOracleConnectionProvider.SupportsNativeSkipTake: Boolean;
begin
  Result := True;
end;

procedure TdxOracleConnectionProvider.ClearAndFree(AList: TList<TDataSet>);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    AList[I].Free;
  AList.Free;
end;

function TdxOracleConnectionProvider.GetDataForTables(const ATables: TArray<TdxDBTable>;
  AFilter: TdxOracleConnectionProvider.TTableFilter; const AQueryText: string): TList<TDataSet>;
var
  AInGroup: string;
  ATable: TdxDBTable;
  AQuery: TdxQuery;
  ACommandText: string;
begin
  Result := TList<TDataSet>.Create;
  AInGroup := '';
  for ATable in ATables do
    if not Assigned(AFilter) or AFilter(ATable) then
      AInGroup := AInGroup + '''' + ComposeSafeTableName(ATable.Name) + ''',';
  if AInGroup <> '' then
  begin
    SetLength(AInGroup, Length(AInGroup) - 1);
    ACommandText := Format(AQueryText, [AInGroup]);
    AQuery := TdxQuery.Create(ACommandText);
    try
      Result.Add(SelectData(AQuery));
    finally
      AQuery.Free;
    end;
  end;

end;

procedure TdxOracleConnectionProvider.GetColumns(ATable: TdxDBTable);
var
  ASchemaName, ASafeTableName: string;
  ADataSet: TDataSet;
  ASize, AScale: Integer;
  ANullable: Boolean;
  ADataDefault: Variant;
  APrecision: TdxNativeInt;
  AColumn: TdxDBColumn;
  AType: TdxDBColumnType;
begin
  ASchemaName := ComposeSafeSchemaName(ATable.Name);
  ASafeTableName := ComposeSafeTableName(ATable.Name);
  if ASchemaName = '' then
    ADataSet := GetDataSet(Format('SELECT COLUMN_NAME, DATA_TYPE, CHAR_COL_DECL_LENGTH, DATA_PRECISION, DATA_SCALE, NULLABLE, DATA_DEFAULT from USER_TAB_COLUMNS where TABLE_NAME = ''%s''', [ASafeTableName]))
  else
    ADataSet := GetDataSet(Format('SELECT COLUMN_NAME, DATA_TYPE, CHAR_COL_DECL_LENGTH, DATA_PRECISION, DATA_SCALE, NULLABLE, DATA_DEFAULT from ALL_TAB_COLUMNS where OWNER = ''%s'' and TABLE_NAME = ''%s''', [ASchemaName, ASafeTableName]));
  try
    while not ADataSet.Eof do
    begin
      if not ADataSet.Fields[2].IsNull then
        ASize := ADataSet.Fields[2].AsInteger
      else
        ASize := 0;
      if not ADataSet.Fields[3].IsNull then
        APrecision := ADataSet.Fields[3].AsInteger
      else
        APrecision := -1;

      if not ADataSet.Fields[4].IsNull then
        AScale := ADataSet.Fields[4].AsInteger
      else
        AScale := 0;

      if not ADataSet.Fields[5].IsNull then
        ANullable := ADataSet.Fields[5].AsString = 'Y'
      else
        ANullable := False;

      if not ADataSet.Fields[6].IsNull then
        ADataDefault := not ADataSet.Fields[6].Value
      else
        ADataDefault := Null;

      AType := GetTypeFromString(ADataSet.Fields[1].AsString, ASize, APrecision, AScale);
      if AType <> TdxDBColumnType.String then
        ASize := 0;
      AColumn := TdxDBColumn.Create(ADataSet.Fields[0].AsString, False, '', ASize, AType);
      AColumn.IsNullable := ANullable;
      AColumn.DefaultValue := ADataDefault;
      ATable.AddColumn(AColumn);
      ADataSet.Next;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TdxOracleConnectionProvider.GetForeignKeys(ATable: TdxDBTable);
const
  ColumnsMap: array [Boolean] of string = ('ALL_CONS_COLUMNS', 'USER_CONS_COLUMNS');
var
  ACommandText, ASchemaName, ASafeTableName: string;
  ADataSet:  TDataSet;
  AForeignKey: TdxDBForeignKey;
begin
  ADataSet := nil;
  try
    ASchemaName := ComposeSafeSchemaName(ATable.Name);
    ASafeTableName := ComposeSafeTableName(ATable.Name);
    if ASchemaName = '' then
    begin
      repeat
        ACommandText:= Format('select tc.POSITION, tc.COLUMN_NAME, fc.COLUMN_NAME, fc.TABLE_NAME from USER_CONSTRAINTS c'#13#10 +
            'join USER_CONS_COLUMNS tc  on tc.CONSTRAINT_NAME = c.CONSTRAINT_NAME '#13#10 +
            'join %s fc on c.R_CONSTRAINT_NAME = fc.CONSTRAINT_NAME and tc.POSITION = fc.POSITION '#13#10 +
            'where c.TABLE_NAME = ''%s'''#13#10 +
            'order by c.CONSTRAINT_NAME, tc.POSITION', [ColumnsMap[FGetForeignKeysHasNoRights], ASafeTableName]);
        try
          ADataSet := GetDataSet(ACommandText);
        except
          if FGetForeignKeysHasNoRights then
            raise;
          FGetForeignKeysHasNoRights := True;
        end;
      until ADataSet <> nil;
    end
    else
    begin
      ACommandText := Format('select tc.POSITION, tc.COLUMN_NAME, fc.COLUMN_NAME, fc.TABLE_NAME from ALL_CONSTRAINTS c'#13#10 +
        'join ALL_CONS_COLUMNS tc  on tc.CONSTRAINT_NAME = c.CONSTRAINT_NAME '#13#10 +
        'join ALL_CONS_COLUMNS fc on c.R_CONSTRAINT_NAME = fc.CONSTRAINT_NAME and tc.POSITION = fc.POSITION '#13#10 +
        'where c.OWNER = ''%s'' and c.TABLE_NAME = ''%s'''#13#10 +
        'order by c.CONSTRAINT_NAME, tc.POSITION', [ASchemaName, ASafeTableName]);
      ADataSet := GetDataSet(ACommandText);
    end;

    AForeignKey := nil;
    while not ADataSet.Eof do
    begin
      if ADataSet.Fields[0].AsInteger = 1 then
      begin
        AForeignKey := TdxDBForeignKey.Create([ADataSet.Fields[1].AsString], ADataSet.Fields[3].AsString, [ADataSet.Fields[2].AsString]);
        ATable.ForeignKeys.Add(AForeignKey);
      end
      else
      begin
        AForeignKey.Columns.Add(ADataSet.Fields[1].AsString);
        AForeignKey.PrimaryKeyTableKeyColumns.Add(ADataSet.Fields[2].AsString);
      end;
      ADataSet.Next;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TdxOracleConnectionProvider.GetIndexes(ATable: TdxDBTable);
var
  ASchemaName, ASafeTableName: string;
  ADataSet: TDataSet;
  AIndex: TdxDBIndex;
begin
  ASchemaName := ComposeSafeSchemaName(ATable.Name);
  ASafeTableName := ComposeSafeTableName(ATable.Name);
  if ASchemaName = '' then
  begin
    ADataSet := GetDataSet(Format('select ind.INDEX_NAME, cols.COLUMN_NAME, cols.COLUMN_POSITION, ind.uniqueness'#13#10 +
      'from USER_INDEXES ind'#13#10 +
      'join USER_IND_COLUMNS cols on ind.INDEX_NAME = cols.INDEX_NAME'#13#10 +
      'where ind.TABLE_NAME = ''%s'''#13#10 +
      'order by ind.INDEX_NAME, cols.COLUMN_POSITION', [ASafeTableName]));
  end
  else
  begin
    ADataSet := GetDataSet(Format('select ind.INDEX_NAME, cols.COLUMN_NAME, cols.COLUMN_POSITION, ind.uniqueness'#13#10 +
    'from ALL_INDEXES ind'#13#10 +
    'join ALL_IND_COLUMNS cols on ind.INDEX_NAME = cols.INDEX_NAME'#13#10 +
    'where ind.TABLE_OWNER = ''%s'' and ind.TABLE_NAME = ''%s'''#13#10 +
    'order by ind.INDEX_NAME, cols.COLUMN_POSITION', [ASchemaName, ASafeTableName]));
  end;
  try
    AIndex := nil;
    while not ADataSet.Eof do
    begin
      if ADataSet.Fields[2].AsInteger = 1 then
      begin
        AIndex := TdxDBIndex.Create(ADataSet.Fields[0].AsString, [ADataSet.Fields[1].AsString], ADataSet.Fields[3].AsString = 'UNIQUE');
        ATable.Indexes.Add(AIndex);
      end
      else
        AIndex.Columns.Add(ADataSet.Fields[1].AsString);
      ADataSet.Next;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TdxOracleConnectionProvider.GetPrimaryKey(ATable: TdxDBTable);
var
  ASchemaName, ASafeTableName, AColumnName: string;
  ADataSet: TDataSet;
  AColumns: TList<string>;
  AColumn: TdxDBColumn;
begin
  ASchemaName := ComposeSafeSchemaName(ATable.Name);
  ASafeTableName := ComposeSafeTableName(ATable.Name);
  if ASchemaName = '' then
    ADataSet := GetDataSet(Format('select tc.COLUMN_NAME from USER_CONS_COLUMNS tc'#13#10 +
    'left join USER_CONSTRAINTS c on tc.CONSTRAINT_NAME = c.CONSTRAINT_NAME'#13#10 +
    'where c.CONSTRAINT_TYPE = ''P'' and tc.TABLE_NAME = ''%s''', [ASafeTableName]))
  else
    ADataSet := GetDataSet(Format('select tc.COLUMN_NAME from ALL_CONS_COLUMNS tc'#13#10 +
    'left join ALL_CONSTRAINTS c on tc.CONSTRAINT_NAME = c.CONSTRAINT_NAME'#13#10 +
    'where c.CONSTRAINT_TYPE = ''P'' and c.OWNER = ''%s''  and tc.TABLE_NAME = ''%s''', [ASchemaName, ASafeTableName]));

  AColumns := TList<string>.Create;
  try
    while not ADataSet.Eof do
    begin
      AColumnName := ADataSet.Fields[0].AsString;
      AColumn := ATable.GetColumn(AColumnName);
      if AColumn <> nil then
        AColumn.IsKey := True;
      AColumns.Add(AColumnName);
      ADataSet.Next;
    end;
    ATable.PrimaryKey := TdxDBPrimaryKey.Create(ATable.Name, AColumns.ToArray, True);
  finally
    AColumns.Free;
    ADataset.Free;
  end;
end;

function TdxOracleConnectionProvider.GetSeqNameCore(ATable: TdxDBTable): string;
begin
  Result := ATable.SequenceName;
  if Result = '' then
    Result := ComposeSafeConstraintName('sq_' + ComposeSafeTableName(ATable.Name));
end;

function TdxOracleConnectionProvider.GetSeqName(ATable: TdxDBTable): string;
var
  ASchemaName, ASqname: string;
begin
  ASchemaName := ComposeSafeSchemaName(ATable.Name);
  ASqname := GetSeqNameCore(ATable);
  if ASchemaName = '' then
    Result := '"' + ASqname + '"'
  else
    Result := '"' + ASchemaName + '"."' + ASqname + '"';
end;

function TdxOracleConnectionProvider.GetTypeFromString(const ATypeName: string; ASize: Integer; APrecision, AScale: Integer): TdxDBColumnType;
var
  ATypeNameLo: string;
begin
  ATypeNameLo := LowerCase(ATypeName);
  if ATypeNameLo = 'int' then
    Result := TdxDBColumnType.Int32
  else if (ATypeNameLo = 'blob') or (ATypeNameLo = 'raw') then
    Result := TdxDBColumnType.ByteArray
  else if (ATypeNameLo = 'number') then
  begin
    if AScale <> 0 then
      Result := TdxDBColumnType.Decimal
    else
    begin
      case APrecision of
        0: Result := TdxDBColumnType.Decimal;
        1: Result := TdxDBColumnType.Boolean;
        2..3: Result := TdxDBColumnType.Byte;
        4..5: Result := TdxDBColumnType.Int16;
        -1, 6..10: Result := TdxDBColumnType.Int32;
        11..20: Result := TdxDBColumnType.Int64;
      else
        Result := TdxDBColumnType.Decimal;
      end;
    end;
  end else if (ATypeNameLo = 'nchar') or (ATypeNameLo = 'char') then
  begin
    if ASize > 1 then
      Result := TdxDBColumnType.String
    else
      Result := TdxDBColumnType.Char;
  end else if ATypeNameLo = 'money' then
    Result := TdxDBColumnType.Decimal
  else if ATypeNameLo = 'float' then
    Result := TdxDBColumnType.Double
  else if (ATypeNameLo = 'nvarchar') or (ATypeNameLo = 'varchar') or (ATypeNameLo = 'varchar2') or (ATypeNameLo = 'nvarchar2') then
    Result := TdxDBColumnType.String
  else if ATypeNameLo = 'date' then
    Result := TdxDBColumnType.DateTime
  else if (ATypeNameLo = 'clob') or (ATypeNameLo = 'nclob') then
    Result := TdxDBColumnType.String
  else
    Result := TdxDBColumnType.Unknown;

{
  case ATypeName.ToLower of
    'int':
      Exit(TdxDBColumnType.Int32);
    'blob',
    'raw':
      Exit(TdxDBColumnType.ByteArray);
    'number':
      begin
        if (APrecision = 0) or (AScale <> 0) then
          Exit(TdxDBColumnType.Decimal);
        if APrecision = 1 then
          Exit(TdxDBColumnType.Boolean);
        if APrecision <= 3 then
          Exit(TdxDBColumnType.Byte);
        if APrecision <= 5 then
          Exit(TdxDBColumnType.Int16);
        if (APrecision = nil) or (APrecision <= 10) then
          Exit(TdxDBColumnType.Int32);
        if APrecision <= 20 then
          Exit(TdxDBColumnType.Int64);
        Exit(TdxDBColumnType.Decimal);
      end;
    'nchar',
    'char':
      if ASize > 1 then
        Exit(TdxDBColumnType.String);
      Exit(TdxDBColumnType.Char);
    'money':
      Exit(TdxDBColumnType.Decimal);
    'float':
      Exit(TdxDBColumnType.Double);
    'nvarchar',
    'varchar',
    'varchar2',
    'nvarchar2':
      Exit(TdxDBColumnType.String);
    'date':
      Exit(TdxDBColumnType.DateTime);
    'clob',
    'nclob':
      Exit(TdxDBColumnType.String);
  end;
  Result := TdxDBColumnType.Unknown;
}
end;

initialization
  TdxSQLConnectionProviderFactory.Register(TdxOracleConnectionProvider);

finalization
  TdxSQLConnectionProviderFactory.Unregister(TdxOracleConnectionProvider);

end.

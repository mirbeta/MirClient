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

unit dxEMF.DB.MySQL;

{$HPPEMIT '#pragma link "dxEMF.DB.MySQL"'}

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, TypInfo, Generics.Collections, DB, Rtti, Variants,
  //
  dxCore,
  //
  dxEMF.DB.Model,
  dxEMF.Types,
  dxEMF.Utils,
  dxEMF.DB.Criteria,
  dxEMF.DB.Query,
  dxEMF.DB.SQLConnectionProvider,
  dxEMF.Strs;

type

  { TdxMySQLConnectionProvider }

  TdxMySQLConnectionProvider = class(TdxSQLConnectionProvider)
  strict private const
    StopChars: array [0 .. 1] of Char = ('_', '%');
  strict private
    class var FConnectionParameters: TDictionary<string, TdxConnectionParameterValues>;
    class var FDBTypes: TDictionary<string, TdxDBColumnType>;

    class procedure PopulateConnectionParameters; static;
    class procedure PopulateDBTypes; static;
  strict private
    FIsVersion5: TdxDefaultBoolean;

    function GetFieldValue(ADataSet: TDataSet; AFieldIndex: Integer): Variant;
    function GetIsVersion5: Boolean;
  protected
    class function GetConnectionParameters: TDictionary<string, TdxConnectionParameterValues>; static;
    class function GetDBTypes: TDictionary<string, TdxDBColumnType>; override;

    function ConvertToDBParameter(const AParameterValue: TValue): TValue; override;
    function GetIdentity(ASQL: TdxQuery): Int64; override;
    function GetMySqlServerVersion: string;

    function GetSQLCreateColumnTypeForBoolean(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForByteArray(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForChar(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForDateTime(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForDecimal(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForDouble(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForGuid(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForSByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForSingle(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForString(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForUInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForUInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForUInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>; override;
    function GetMaximumStringSize: Integer; override;
    function GetSafeNameTableMaxLength: Integer; override;

    procedure GetColumns(ATable: TdxDBTable);
    procedure GetForeignKeys(ATable: TdxDBTable);
    procedure GetIndexes(ATable: TdxDBTable);
    procedure GetPrimaryKey(ATable: TdxDBTable);
    function GetTypeFromString(const ATypeName: string; out ASize: Integer): TdxDBColumnType;

    class property ConnectionParameters: TDictionary<string, TdxConnectionParameterValues> read GetConnectionParameters;
  public
    constructor Create(AConnectionVendor: TdxCustomConnectionVendor; AAutoCreateOption: TdxAutoCreateOption); override;
    class destructor Finalize;
    class function GetDBEngine: TdxDBEngine; override;

    function FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string; override;
    function FormatColumn(const AColumnName, ATableAlias: string): string; override;
    function FormatColumn(const AColumnName: string): string; override;
    function FormatConstraint(const AConstraintName: string): string; override;
    function FormatCreateTable(const ATableName: string; const AColumns: string): string; override;
    function FormatDelete(const ATableName: string; const AWhereClause: string): string; override;
    function FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string; override;
    function FormatFunction(AProcessParameter: TdxProcessParameter;
      AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string; override;
    function FormatInsert(const ATableName: string; const AFields: string; const AValues: string): string; override;
    function FormatInsertDefaultValues(ATable: TdxDBTable): string; override;
    function FormatSelect(const ASelectedPropertiesSQL, AFromSQL, AWhereSQL, AOrderBySQL, AGroupBySQL, AHavingSQL: string;
      ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer): string; override;
    function FormatTable(const ASchemaName, ATableName: string): string; override;
    function FormatTable(const ASchemaName, ATableName, ATableAlias: string): string; override;
    function FormatUpdate(const ATableName: string; const ASets: string; const AWhereClause: string): string; override;
    function GetConnectionParameter(const AConnectionType: TdxConnectionType; AParameter: TdxConnectionParameter): string; override;
    function GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer; var ACreateParameter: Boolean): string; override;
    function GetSQLCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetStorageTableNames(AIncludeViews: Boolean): TArray<string>; override;
    procedure GetTableSchema(ATable: TdxDBTable; ACheckIndexes: Boolean; ACheckForeignKeys: Boolean); override;
    function SupportsNativeSkipTake: Boolean; override;

    class function FnAddDateTime(const ADatetimeOperand: string; const ADayPart: string; const ASecondPart: string): string; static;
    class function FormatGetInt(const AArg: string; AMultiplier, ADivider: Integer): string; static;
    class function FormatMod(const AArg: string; AMultiplier, ADivider: Integer): string; static;

    property IsVersion5: Boolean read GetIsVersion5;
  end;

implementation

uses
  StrUtils, dxStringHelper;

{ TdxMySQLConnectionProvider }

constructor TdxMySQLConnectionProvider.Create(
  AConnectionVendor: TdxCustomConnectionVendor; AAutoCreateOption: TdxAutoCreateOption);
begin
  inherited;
  FIsVersion5 := bDefault;
end;

class destructor TdxMySQLConnectionProvider.Finalize;
begin
  FreeAndNil(FConnectionParameters);
  FreeAndNil(FDBTypes);
end;

class function TdxMySQLConnectionProvider.GetDBEngine: TdxDBEngine;
begin
  Result := TdxDBEngines.MySQL;
end;

function TdxMySQLConnectionProvider.FormatBinary(
  AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string;
begin
  if AOperatorType = TdxBinaryOperatorType.Modulo then
    Result := ALeftOperand + ' % ' + ARightOperand
  else
    Result := inherited FormatBinary(AOperatorType, ALeftOperand, ARightOperand);
end;

function TdxMySQLConnectionProvider.FormatColumn(const AColumnName: string): string;
begin
  Result := Format('`%s`', [AColumnName]);
end;

function TdxMySQLConnectionProvider.FormatColumn(const AColumnName, ATableAlias: string): string;
begin
  Result := Format('%1:s.`%0:s`', [AColumnName, ATableAlias]);
end;

function TdxMySQLConnectionProvider.FormatConstraint(const AConstraintName: string): string;
begin
  Result := Format('`%s`', [AConstraintName]);
end;

function TdxMySQLConnectionProvider.FormatCreateTable(const ATableName: string; const AColumns: string): string;
begin
  Result := Format('create table if not exists %s (%s) CHARACTER SET utf8 COLLATE utf8_unicode_ci;', [ATableName, AColumns]);
end;

function TdxMySQLConnectionProvider.FormatDelete(const ATableName, AWhereClause: string): string;
begin
  Result := 'delete from ' + ATableName + ' where ' + AWhereClause;
end;

function TdxMySQLConnectionProvider.FormatFunction(
  AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string;
var
  ACounter: Integer;
  AIndex: Integer;
  AStringBuilder: TStringBuilder;
begin
  Result := '';
  case AOperatorType of
   TdxFunctionOperatorType.MillisecondOf,
   TdxFunctionOperatorType.ToDouble,
   TdxFunctionOperatorType.ToSingle:
      raise ENotSupportedException.Create('');

    TdxFunctionOperatorType.Len:
      Result := Format('LENGTH(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Trim:
      Result := Format('Trim(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.UpperCase:
      Result := Format('Upper(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.LowerCase:
      Result := Format('Lower(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Ascii:
      Result := Format('Ascii(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Char:
      Result := Format('cast(Char(%0:s) as char(1))', [AOperands[0]]);
    TdxFunctionOperatorType.ToInteger:
      Result := Format('Cast(%0:s as signed)', [AOperands[0]]);
    TdxFunctionOperatorType.ToInt64:
      Result := Format('Cast(%0:s as decimal(20, 0))', [AOperands[0]]);
    TdxFunctionOperatorType.ToDecimal:
      Result := Format('Cast(%0:s as decimal(65,30))', [AOperands[0]]);
    TdxFunctionOperatorType.ToString:
      Result := Format('Cast(%0:s as char)', [AOperands[0]]);
    TdxFunctionOperatorType.Replace:
      Result := Format('Replace(%0:s,%1:s,%2:s)', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Reverse:
      Result := Format('Reverse(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Insert:
      Result := Format('Concat(Left(%0:s,%1:s),%2:s,Right(%0:s,Length(%0:s)-(%1:s)))', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Abs:
      Result := Format('Abs(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.BigMul:
      Result := Format('(cast(%0:s as signed int) * cast(%1:s as signed int))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Sqr:
      Result := Format('Sqrt(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sinh:
      Result := Format('( (Exp(%0:s) - Exp((%0:s * (-1) ))) / 2 )', [AOperands[0]]);
    TdxFunctionOperatorType.Cosh:
      Result := Format('( (Exp(%0:s) + Exp((%0:s * (-1) ))) / 2 )', [AOperands[0]]);
    TdxFunctionOperatorType.Tanh:
      Result := Format('( (Exp(%0:s) - Exp((%0:s * (-1) ))) / (Exp(%0:s) + Exp((%0:s * (-1) ))) )', [AOperands[0]]);
    TdxFunctionOperatorType.Random:
      Result := 'Rand()';
    TdxFunctionOperatorType.Log10:
      Result := Format('Log10(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sin:
      Result := Format('Sin(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcSin:
      Result := Format('Asin(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Tan:
      Result := Format('Tan(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan:
      Result := Format('Atan(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan2:
      Result := Format('Atan2(%0:s,%1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Cos:
      Result := Format('Cos(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcCos:
      Result := Format('Acos(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Exp:
      Result := Format('Exp(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Power:
      Result := Format('Power(%0:s,%1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Max:
      Result := Format('if(%0:s > %1:s, %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Min:
      Result := Format('if(%0:s < %1:s, %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Sign:
      Result := Format('Sign(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Floor:
      Result := Format('Floor(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.Ceil:
      Result := Format('Ceiling(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.IsNullOrEmpty:
      Result := Format('((%0:s) is null or length(%0:s) = 0)', [AOperands[0]]);
    TdxFunctionOperatorType.EndsWith:
      Result := Format('(Right(%0:s, Length(%1:s)) = (%1:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Contains:
      Result := Format('(Instr(%0:s, %1:s) > 0)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.SecondOf:
      Result := Format('Second(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.MinuteOf:
      Result := Format('Minute(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.HourOf:
      Result := Format('Hour(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOf:
      Result := Format('Day(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.MonthOf:
      Result := Format('Month(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.YearOf:
      Result := Format('Year(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.GetTimeOfDay:
      Result := Format('((Hour(%0:s)) * 36000000000) + ((Minute(%0:s)) * 600000000) + (Second(%0:s) * 10000000)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheWeek:
      Result := Format('((DayOfWeek(%0:s)- DayOfWeek('#$27'1900-01-01'#$27')  + 8) % 7)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheYear:
      Result := Format('DayOfYear(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.DateOf:
      Result := Format('Date(%0:s)', [AOperands[0]]);
    TdxFunctionOperatorType.IncTick:
      Result := Format('cast(Adddate(%0:s, interval (%1:s div 10000000 ) second )  as datetime)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMillisecond:
      Result := Format('cast(Adddate(%0:s, interval ( %1:s div 1000) second )  as datetime)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.AddTimeSpan, TdxFunctionOperatorType.IncSecond:
      Result := FnAddDateTime(AOperands[0], FormatGetInt(AOperands[1], 1, 86400), FormatMod(AOperands[1], 1, 86400));
    TdxFunctionOperatorType.IncMinute:
      Result := FnAddDateTime(AOperands[0], FormatGetInt(AOperands[1], 60, 86400), FormatMod(AOperands[1], 60, 86400));
    TdxFunctionOperatorType.IncHour:
      Result := FnAddDateTime(AOperands[0], FormatGetInt(AOperands[1], 3600, 86400), FormatMod(AOperands[1], 3600, 86400));
    TdxFunctionOperatorType.IncDay:
      Result := FnAddDateTime(AOperands[0], FormatGetInt(AOperands[1], 86400, 86400), FormatMod(AOperands[1], 86400, 86400));
    TdxFunctionOperatorType.IncMonth:
      Result := Format('cast(Adddate(%0:s, interval %1:s month )  as datetime)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncYear:
      Result := Format('cast(Adddate(%0:s, interval %1:s year )  as datetime)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.YearsBetween:
      Result := Format('(Year(%1:s) - Year(%0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MonthsBetween:
      Result := Format('(((Year(%1:s) - Year(%0:s)) * 12) + Month(%1:s) - Month(%0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.SecondsBetween:
      Result := Format('(UNIX_TIMESTAMP(%1:s) - UNIX_TIMESTAMP(%0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MillisecondsBetween:
      Result := Format('((UNIX_TIMESTAMP(%1:s) - UNIX_TIMESTAMP(%0:s)) * 1000)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.DateDiffTicks:
      Result := Format('((UNIX_TIMESTAMP(%1:s) - UNIX_TIMESTAMP(%0:s))) * 10000000', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Now:
      Result := 'Now()';
    TdxFunctionOperatorType.UtcNow:
      Result := 'UTC_TIMESTAMP()';
    TdxFunctionOperatorType.Today:
      Result := 'CurDate()';

    TdxFunctionOperatorType.DaysBetween:
      if IsVersion5 then
        Result := Format('DATEDIFF(%1:s, %0:s)', [AOperands[0], AOperands[1]])
      else
        Result := Format('(TO_DAYS(%1:s) - TO_DAYS(%0:s))',  [AOperands[0], AOperands[1]]);

    TdxFunctionOperatorType.HoursBetween:
      Result := Format('((%2:s * 24) + Hour(%1:s) - Hour(%0:s))',
        [AOperands[0], AOperands[1], FormatFunction(TdxFunctionOperatorType.DaysBetween, AOperands)]);

    TdxFunctionOperatorType.MinutesBetween:
      Result := Format('((%2:s) * 60 + Minute(%1:s) - Minute(%0:s))',
        [AOperands[0], AOperands[1], FormatFunction(TdxFunctionOperatorType.HoursBetween, AOperands)]);

    TdxFunctionOperatorType.Substring:
      if Length(AOperands) < 3 then
        Result := Format('SUBSTR(%0:s,%1:s + 1)', [AOperands[0], AOperands[1]])
      else
        Result := Format('SUBSTR(%0:s, %1:s + 1, %2:s)', [AOperands[0], AOperands[1], AOperands[2]]);

    TdxFunctionOperatorType.PadLeft:
      case Length(AOperands) of
        2: Result := Format('If(Length(%0:s) >= %1:s, %0:s, LPad(%0:s, %1:s, '#$27' '#$27'))', [AOperands[0], AOperands[1]]);
        3: Result := Format('If(Length(%0:s) >= %1:s, %0:s, LPad(%0:s, %1:s, %2:s))', [AOperands[0], AOperands[1], AOperands[2]]);
      end;

    TdxFunctionOperatorType.PadRight:
      case Length(AOperands) of
        2: Result := Format('If(Length(%0:s) >= %1:s, %0:s,RPad(%0:s, %1:s, '#$27' '#$27'))', [AOperands[0], AOperands[1]]);
        3: Result := Format('If(Length(%0:s) >= %1:s, %0:s,RPad(%0:s, %1:s, %2:s))', [AOperands[0], AOperands[1], AOperands[2]]);
      end;

    TdxFunctionOperatorType.CharIndex:
      case Length(AOperands) of
        2: Result := Format('(Instr(%1:s,%0:s)-1)', [AOperands[0], AOperands[1]]);
        3: Result := Format('( if( Instr(Right(%1:s,(Length(%1:s) - %2:s)),%0:s) = 0, -1 , Instr(Right(%1:s,(Length(%1:s) - %2:s)),%0:s) -1 + %2:s) )', [AOperands[0], AOperands[1], AOperands[2]]);
        4: Result := Format('(if( instr(Left( Right(%1:s,(Length(%1:s) - %2:s ) ) ,{3}),%0:s) = 0, -1, instr(Left( Right(%1:s,(Length(%1:s) - %2:s ) ) ,{3}),%0:s) - 1 + %2:s))', [AOperands[0], AOperands[1], AOperands[2], AOperands[3]]);
      end;

    TdxFunctionOperatorType.Remove:
      case Length(AOperands) of
        2: Result := Format('Left(%0:s,%1:s)', [AOperands[0], AOperands[1]]);
        3: Result := Format('Concat(Left(%0:s,%1:s),Right(%0:s,Length(%0:s)-%1:s-%2:s))', [AOperands[0], AOperands[1], AOperands[2]]);
      end;

    TdxFunctionOperatorType.Log:
      case Length(AOperands) of
        1: Result := Format('Log(%0:s)', [AOperands[0]]);
        2: Result := Format('Log(%1:s,%0:s)', [AOperands[0], AOperands[1]]);
      end;

     TdxFunctionOperatorType.Round:
      case Length(AOperands) of
        1: Result := Format('Round(%0:s,0)', [AOperands[0]]);
        2: Result := Format('Round(%0:s,%1:s)', [AOperands[0], AOperands[1]]);
      end;

    TdxFunctionOperatorType.IsNull:
      case Length(AOperands) of
        1: Result := Format('(%0:s is null)', [AOperands[0]]);
        2: Result := Format('Coalesce(%0:s,%1:s)', [AOperands[0], AOperands[1]]);
      end;

    TdxFunctionOperatorType.Concat:
      begin
        AStringBuilder := TStringBuilder.Create;
        try
          for AIndex := 0 to Length(AOperands) - 1 do
            if Length(AOperands[AIndex]) > 0 then
            begin
              if AStringBuilder.Length > 0 then
                AStringBuilder.Append(', ');
              AStringBuilder.Append(AOperands[AIndex]);
            end;
          Result := Format('CONCAT(%0:s)', [AStringBuilder.ToString]);
        finally
          AStringBuilder.Free;
        end;
      end;

    TdxFunctionOperatorType.Iif:
      begin
        if (Length(AOperands) < 3) or (Length(AOperands) mod 2 = 0) then
          raise EArgumentException.Create(sdxFilteringTheIifFunctionOperatorRequiresThree);
        if Length(AOperands) = 3 then
          Exit(Format('If(%0:s, %1:s, %2:s)', [AOperands[0], AOperands[1], AOperands[2]]));

        AStringBuilder := TStringBuilder.Create;
        try
          AIndex := -2;
          ACounter := 0;
          repeat
            Inc(AIndex, 2);
            AStringBuilder.AppendFormat('If(%0:s, %1:s, ', [AOperands[AIndex], AOperands[AIndex + 1]]);
            Inc(ACounter);
          until AIndex + 3 >= Length(AOperands);
          AStringBuilder.AppendFormat('%0:s', [AOperands[AIndex + 2]]);
          AStringBuilder.Append(DupeString(')', ACounter));

          Result := AStringBuilder.ToString;
        finally
          AStringBuilder.Free;
        end;
      end;

  end;

  if Result = '' then
    Result := inherited FormatFunction(AOperatorType, AOperands);
end;

function TdxMySQLConnectionProvider.FormatFunction(AProcessParameter: TdxProcessParameter;
  AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string;
var
  AConstantValue: IdxCriteriaOperator;
  ALikeIndex: Integer;
  AOperandString: string;
  ASecondOperand: TdxCriteriaOperator;
begin
  if AOperatorType = TdxFunctionOperatorType.StartsWith then
  begin
    ASecondOperand := Safe<TdxCriteriaOperator>.Cast(AOperands[1].AsObject);
    if (ASecondOperand is TdxOperandValue) and ((TdxOperandValue(ASecondOperand)).Value.IsString) then
    begin
      AOperandString := TdxOperandValue(ASecondOperand).Value.AsString;
      ALikeIndex := TdxStringHelper.IndexOfAny(AOperandString, StopChars);
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
          AConstantValue := TdxConstantValue.Create(TdxStringHelper.Substring(AOperandString, 0, ALikeIndex) + '%');
          Exit(Format('((%0:s like %2:s) And (Left(%0:s, Len(%1:s)) = (%1:s)))',
            [AProcessParameter(Safe<TdxCriteriaOperator>.Cast(AOperands[0].AsObject)),
            AProcessParameter(ASecondOperand),
            AProcessParameter(AConstantValue)]));
        end;
    end;
    Result := Format('(Left(%0:s, Len(%1:s)) = (%1:s))',
      [AProcessParameter(Safe<TdxCriteriaOperator>.Cast(AOperands[0].AsObject)),
      AProcessParameter(ASecondOperand)]);
  end
  else
    Result := inherited FormatFunction(AProcessParameter, AOperatorType, AOperands);
end;

function TdxMySQLConnectionProvider.FormatInsert(const ATableName, AFields, AValues: string): string;
begin
  Result := 'insert into ' + ATableName + '(' + AFields + ')values(' + AValues + ')';
end;

function TdxMySQLConnectionProvider.FormatInsertDefaultValues(ATable: TdxDBTable): string;
begin
  Result := Format('insert into %s values()', [FormatTableSafe(ATable)]);
end;

function TdxMySQLConnectionProvider.FormatSelect(
  const ASelectedPropertiesSQL, AFromSQL, AWhereSQL, AOrderBySQL, AGroupBySQL, AHavingSQL: string;
  ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer): string;

  function FormatPartIfNotEmpty(const AFormatString, AValue: string): string;
  begin
    if AValue <> '' then
      Result := Format(AFormatString, [AValue])
    else
      Result := '';
  end;

var
  AModificatorsSql: string;
  ATopSelectedRecordsValue: Integer;
begin
  inherited FormatSelect(ASelectedPropertiesSql, AFromSql, AWhereSql,
    AOrderBySql, AGroupBySql, AHavingSql, ASkipSelectedRecords, ATopSelectedRecords);

  AModificatorsSql := '';
  if ASkipSelectedRecords = 0 then
  begin
    if ATopSelectedRecords <> 0 then
      AModificatorsSql := Format('limit %d ', [ATopSelectedRecords]);
  end
  else
  begin
    if ATopSelectedRecords = 0 then
      ATopSelectedRecordsValue := MaxInt
    else
      ATopSelectedRecordsValue := ATopSelectedRecords;

    AModificatorsSql := Format('limit %0:d, %1:d ', [ASkipSelectedRecords, ATopSelectedRecordsValue]);
  end;

  Result := Format('select %1:s from %2:s%3:s%4:s%5:s%6:s %0:s', [
    AModificatorsSql, ASelectedPropertiesSql, AFromSql,
    FormatPartIfNotEmpty(#10'where %s', AWhereSql),
    FormatPartIfNotEmpty(#10'group by %s', AGroupBySql),
    FormatPartIfNotEmpty(#10'having %s', AHavingSql),
    FormatPartIfNotEmpty(#10'order by %s', AOrderBySql)]);
end;

function TdxMySQLConnectionProvider.FormatTable(const ASchemaName, ATableName: string): string;
begin
  Result := Format('`%s`', [ATableName]);
end;

function TdxMySQLConnectionProvider.FormatTable(const ASchemaName, ATableName, ATableAlias: string): string;
begin
  Result := Format('`%0:s` %1:s', [ATableName, ATableAlias]);
end;

function TdxMySQLConnectionProvider.FormatUpdate(const ATableName, ASets, AWhereClause: string): string;
begin
  Result := 'update ' + ATableName + ' set ' + ASets + ' where ' + AWhereClause;
end;

function TdxMySQLConnectionProvider.GetConnectionParameter(
  const AConnectionType: TdxConnectionType; AParameter: TdxConnectionParameter): string;
begin
  Result := ConnectionParameters[AConnectionType][AParameter];
end;

function TdxMySQLConnectionProvider.GetMaximumStringSize: Integer;
begin
  Result := 255;
end;

function TdxMySQLConnectionProvider.GetParameterName(const AParameter: IdxOperandValue;
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
        Exit(Concat('N'#$27, TdxStringHelper.Replace(AValue.AsString, #$27, #$27#$27), #$27));
    end;
  end;

  ACreateParameter := True;
  Result := GetParamAlias(AIndex);
end;

function TdxMySQLConnectionProvider.GetSQLCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := GetSqlCreateColumnType(ATable, AColumn);
  if AColumn.ColumnType <> TdxDBColumnType.Boolean then
  begin
    if AColumn.IsKey or not AColumn.IsNullable then
      Result := Result + ' NOT NULL'
    else
      Result := Result + ' NULL';

    if AColumn.IsKey and AColumn.IsIdentity and (AColumn.ColumnType in [TdxDBColumnType.Int32, TdxDBColumnType.Int64]) and IsSingleColumnPKColumn(ATable, AColumn) then
      Result := Result + ' AUTO_INCREMENT PRIMARY KEY';
  end;
end;

class function TdxMySQLConnectionProvider.GetConnectionParameters: TDictionary<string, TdxConnectionParameterValues>;
begin
  if FConnectionParameters = nil then
  begin
    FConnectionParameters := TDictionary<string, TdxConnectionParameterValues>.Create;
    PopulateConnectionParameters;
  end;
  Result := FConnectionParameters;
end;

class function TdxMySQLConnectionProvider.GetDBTypes: TDictionary<string, TdxDBColumnType>;
begin
  if FDBTypes = nil then
  begin
    FDBTypes := TDictionary<string, TdxDBColumnType>.Create;
    PopulateDBTypes;
  end;
  Result := FDBTypes;
end;

function TdxMySqlConnectionProvider.GetIdentity(ASQL: TdxQuery): Int64;
begin
  Execute(ASQL);
  Result := ConnectionVendor.ExecuteScalar('select last_insert_id();');
end;

function TdxMySqlConnectionProvider.GetMySqlServerVersion: string;
begin
  Result := ConnectionVendor.ExecuteScalar('select version();');
end;

function TdxMySqlConnectionProvider.ConvertToDBParameter(const AParameterValue: TValue): TValue;
begin
  if AParameterValue.IsType<TGUID> then
    Result := Copy(GUIDToString(AParameterValue.AsType<TGUID>), 2, 36)
  else
    Result := inherited ConvertToDBParameter(AParameterValue);
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForBoolean(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'bit';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'tinyint unsigned';
end;

function TdxMySQLConnectionProvider.GetSQLCreateColumnTypeForByteArray(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  if (AColumn.Size <= 0) or (AColumn.Size > 16777215) then
    Exit('LONGBLOB');
  if AColumn.Size > 65535 then
    Exit('MEDIUMBLOB');
  if AColumn.Size > 127 then
    Exit('BLOB');
  Result := 'TINYBLOB';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForSByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'tinyint';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForChar(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'char';
end;

function TdxMySQLConnectionProvider.GetSQLCreateColumnTypeForDateTime(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'datetime';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForDecimal(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'decimal(28,8)';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForDouble(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'double';
end;

function TdxMySQLConnectionProvider.GetSQLCreateColumnTypeForGuid(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'char(38)';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForSingle(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'real';
end;

function TdxMySQLConnectionProvider.GetSQLCreateColumnTypeForString(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
var
  ASize: Integer;
begin
  ASize := AColumn.Size;
  if ASize = 0 then
    ASize := DefaultStringSize;

  if (ASize < 0) or (ASize > 16777215) then
    Exit('LONGTEXT');
  if ASize > 65535 then
    Exit('MEDIUMTEXT');
  if ASize > 255 then
    Exit('TEXT');
  Result := 'varchar(' + IntToStr(ASize) + ')';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'int';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForUInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'int unsigned';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'smallint';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForUInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'smallint unsigned';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'bigint';
end;

function TdxMySqlConnectionProvider.GetSqlCreateColumnTypeForUInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'bigint unsigned';
end;

function TdxMySQLConnectionProvider.CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>;
const
  V5Query = 'select TABLE_NAME, TABLE_TYPE from INFORMATION_SCHEMA.TABLES where table_schema = '#$27 +
    '%0:s'#$27' and TABLE_TYPE in ('#$27'BASE TABLE'#$27', '#$27'VIEW'#$27')';
var
  ADataSet: TDataSet;
  AQuery: TdxQuery;
  AResult: TDictionary<string, TdxDBTable>;
  ARowValues0: string;
  ATable: TdxDBTable;
  I: Integer;
begin
  AResult := TDictionary<string, TdxDBTable>.Create;
  try
    for I := 0 to Length(ATables) - 1 do
      AResult.AddOrSetValue(LowerCase(ComposeSafeTableName(ATables[I].Name)), ATables[I]);

    if IsVersion5 then
    begin
      AQuery := TdxQuery.Create(Format(V5Query, [ConnectionVendor.GetDatabaseName(Self)]));
      try
        ADataSet := SelectData(AQuery);
        try
          while not ADataSet.Eof do
          begin
            ARowValues0 := LowerCase(GetFieldValue(ADataSet, 0));
            if AResult.TryGetValue(ARowValues0, ATable) then
            begin
              ATable.IsView := GetFieldValue(ADataSet, 1) = 'VIEW';
              AResult.Remove(ARowValues0);
            end;
            ADataSet.Next;
          end;
        finally
          ADataSet.Free;
        end;
      finally
        AQuery.Free;
      end;
    end
    else
    begin
      AQuery := TdxQuery.Create('show tables');
      try
        ADataSet := SelectData(AQuery);
        try
          while not ADataSet.Eof do
          begin
            AResult.Remove(LowerCase(GetFieldValue(ADataSet, 0)));
            ADataSet.Next;
          end;
        finally
          ADataSet.Free;
        end;
      finally
        AQuery.Free;
      end;
    end;

    Result := AResult.Values.ToArray;
  finally
    AResult.Free;
  end;
end;

function TdxMySQLConnectionProvider.GetStorageTableNames(AIncludeViews: Boolean): TArray<string>;

  function BuildFormatLine: string;
  begin
    if IsVersion5 then
      Result := 'select TABLE_NAME from INFORMATION_SCHEMA.TABLES where table_schema = ''%0:s'' and (TABLE_TYPE = ''BASE TABLE'' ' +
        IfThen(AIncludeViews, ' or TABLE_TYPE=''VIEW'')', ')')
    else
      Result := 'show tables from %0:s';
  end;

var
  ADataSet: TDataSet;
  AQuery: TdxQuery;
  AResult: TList<string>;
begin
  AResult := TList<string>.Create;
  AQuery := TdxQuery.Create(Format(BuildFormatLine, [ConnectionVendor.GetDatabaseName(Self)]));
  try
    ADataSet := SelectData(AQuery);
    try
      while not ADataSet.Eof do
      begin
        AResult.Add(GetFieldValue(ADataSet, 0));
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
    Result := AResult.ToArray;
  finally
    AQuery.Free;
    AResult.Free;
  end;
end;

procedure TdxMySQLConnectionProvider.GetTableSchema(ATable: TdxDBTable; ACheckIndexes, ACheckForeignKeys: Boolean);
begin
  GetColumns(ATable);
  GetPrimaryKey(ATable);
  if ACheckIndexes then
    GetIndexes(ATable);
  if ACheckForeignKeys then
    GetForeignKeys(ATable);
end;

function TdxMySqlConnectionProvider.SupportsNativeSkipTake: Boolean;
begin
  Result := True;
end;

class function TdxMySQLConnectionProvider.FnAddDateTime(
  const ADatetimeOperand: string; const ADayPart: string; const ASecondPart: string): string;
begin
  Result := Format('cast(AddDate(AddDate(%0:s, interval %1:s day), interval %2:s second) as datetime)', [ADatetimeOperand, ADayPart, ASecondPart]);
end;

class function TdxMySQLConnectionProvider.FormatMod(const AArg: string; AMultiplier: Integer; ADivider: Integer): string;
begin
  Result := Format('(Truncate(Cast(%0:s as decimal(65,30)) * %1:s, 0) %% %2:s)', [AArg, AMultiplier, ADivider]);
end;

class function TdxMySQLConnectionProvider.FormatGetInt(const AArg: string; AMultiplier: Integer; ADivider: Integer): string;
begin
  Result := Format('(Cast(%0:s as decimal(65,30)) * %1:s Div %2:s)', [AArg, AMultiplier, ADivider]);
end;

function TdxMySQLConnectionProvider.GetSafeNameTableMaxLength: Integer;
begin
  Result := 64;
end;

procedure TdxMySQLConnectionProvider.GetColumns(ATable: TdxDBTable);
var
  AColumn: TdxDBColumn;
  ADataSet: TDataSet;
  AQuery: TdxQuery;
  ASize: Integer;
  AType: TdxDBColumnType;
begin
  AQuery := TdxQuery.Create(Format('show columns from `%0:s`', [ComposeSafeTableName(ATable.Name)]));
  try
    ADataSet := SelectData(AQuery);
    try
      while not ADataSet.Eof do
      begin
        AType := GetTypeFromString(GetFieldValue(ADataSet, 1), ASize);
        if AType <> TdxDBColumnType.String then
          ASize := 0;

        AColumn := TdxDBColumn.Create(GetFieldValue(ADataSet, 0), False, '', ASize, AType);
        AColumn.IsIdentity := ContainsText(GetFieldValue(ADataSet, 5), 'auto_increment');
        AColumn.IsNullable := SameText(GetFieldValue(ADataSet, 2), 'YES');
        AColumn.DefaultValue := GetFieldValue(ADataSet, 4);

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

procedure TdxMySQLConnectionProvider.GetForeignKeys(ATable: TdxDBTable);

  procedure SplitToList(AList: TList<string>; const S: string);
  var
    AParts: TArray<string>;
    I: Integer;
  begin
    AList.Clear;
    AParts := TdxStringHelper.Split(S, [',']);
    for I := 0 to Length(AParts) - 1 do
      AList.Add(TdxStringHelper.Trim(AParts[I], ['`', ' ']));
  end;

var
  AColumnIndex: Integer;
  AColumns: TList<string>;
  ADataSet: TDataSet;
  APos: Integer;
  APrimaryColumns: TList<string>;
  APrimesEndIndex: Integer;
  APrimesIndex: Integer;
  AQuery: TdxQuery;
  ARefsIndex: Integer;
  ARefTable: string;
  AString: string;
  ATempString: string;
begin
  AQuery := TdxQuery.Create(Format('show create table `%0:s`', [ComposeSafeTableName(ATable.Name)]));
  try
    ADataSet := SelectData(AQuery);
    try
      if not ADataSet.IsEmpty then
      begin
        AColumns := TList<string>.Create;
        APrimaryColumns := TList<string>.Create;
        try
          AString := GetFieldValue(ADataSet, 1);
          APos := 0;
          repeat
            APos := TdxStringHelper.IndexOf(AString, 'CONSTRAINT', APos + 1);
            if APos = -1 then
              Break;

            AColumnIndex := TdxStringHelper.IndexOf(AString, 'FOREIGN KEY', APos);
            ARefsIndex := TdxStringHelper.IndexOf(AString, 'REFERENCES', APos);
            if (AColumnIndex < 0) or (ARefsIndex < 0) then
              Break;

            APrimesIndex := TdxStringHelper.IndexOf(AString, '(', ARefsIndex);
            APrimesEndIndex := TdxStringHelper.IndexOf(AString, ')', APrimesIndex);
            if (APrimesIndex < 0) or (APrimesEndIndex < 0) then
              Break;

            ATempString := TdxStringHelper.SubString(AString, ARefsIndex + 12, APrimesIndex - 12 - ARefsIndex);
            ARefTable := TdxStringHelper.Trim(ATempString, ['`', ' ']);

            ATempString := TdxStringHelper.SubString(AString, AColumnIndex + 12, ARefsIndex - 12 - AColumnIndex);
            SplitToList(AColumns, TdxStringHelper.Trim(ATempString, ['`', ' ', '(', ')']));

            ATempString := TdxStringHelper.SubString(AString, APrimesIndex, APrimesEndIndex - APrimesIndex);
            SplitToList(APrimaryColumns, TdxStringHelper.Trim(ATempString, [' ', '(', ')']));

            ATable.AddForeignKey(TdxDBForeignKey.Create(AColumns.ToArray, ARefTable, APrimaryColumns.ToArray));
          until False;
        finally
          APrimaryColumns.Free;
          AColumns.Free;
        end;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQuery.Free;
  end;
end;

procedure TdxMySQLConnectionProvider.GetIndexes(ATable: TdxDBTable);
var
  ADataSet: TDataSet;
  AIndex: TdxDBIndex;
  AIsUnique: Boolean;
  AQuery: TdxQuery;
  ARowValues2: string;
  ARowValues4: string;
begin
  AQuery := TdxQuery.Create(Format('show index from `%0:s`', [ComposeSafeTableName(ATable.Name)]));
  try
    ADataSet := SelectData(AQuery);
    try
      AIndex := nil;
      while not ADataSet.Eof do
      begin
        AIsUnique := GetFieldValue(ADataSet, 1) = 0;
        ARowValues2 := GetFieldValue(ADataSet, 2);
        ARowValues4 := GetFieldValue(ADataSet, 4);
        if (AIndex = nil) or (AIndex.Name <> ARowValues2) then
        begin
          AIndex := TdxDBIndex.Create(ARowValues2, [ARowValues4], AIsUnique);
          ATable.Indexes.Add(AIndex);
        end
        else
          AIndex.Columns.Add(ARowValues4);

        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQuery.Free;
  end;
end;

procedure TdxMySQLConnectionProvider.GetPrimaryKey(ATable: TdxDBTable);
var
  AColumn: TdxDBColumn;
  AColumns: TList<string>;
  ADataSet: TDataSet;
  AQuery: TdxQuery;
  ATopRow2: string;
  ATopRow4: string;
begin
  AQuery := TdxQuery.Create(Format('show index from `%0:s`', [ComposeSafeTableName(ATable.Name)]));
  try
    ADataSet := SelectData(AQuery);
    try
      AColumns := TList<string>.Create;
      try
        while not ADataSet.Eof do
        begin
          ATopRow2 := GetFieldValue(ADataSet, 2);
          ATopRow4 := GetFieldValue(ADataSet, 4);
          if ATopRow2 = 'PRIMARY' then
          begin
            AColumn := ATable.GetColumn(ATopRow4);
            if AColumn <> nil then
              AColumn.IsKey := True;
            AColumns.Add(ATopRow4);
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

function TdxMySQLConnectionProvider.GetTypeFromString(const ATypeName: string; out ASize: Integer): TdxDBColumnType;

  function RemoveBrackets(const ATypeName: string): string;
  var
    ABracketClose: Integer;
    ABracketOpen: Integer;
    ATypeWithoutBrackets: string;
  begin
    ATypeWithoutBrackets := ATypeName;
    ABracketOpen := TdxStringHelper.IndexOf(ATypeName, '(');
    if ABracketOpen >= 0 then
    begin
      ABracketClose := TdxStringHelper.IndexOf(ATypeName, ')', ABracketOpen);
      if ABracketClose >= 0 then
        ATypeWithoutBrackets := TdxStringHelper.Remove(ATypeName, ABracketOpen, ABracketClose - ABracketOpen + 1);
    end;
    Result := ATypeWithoutBrackets;
  end;

begin
  ASize := 0;
  if not DBTypes.TryGetValue(LowerCase(ATypeName), Result) then
  begin
    if not DBTypes.TryGetValue(LowerCase(RemoveBrackets(ATypeName)), Result) then
    begin
      if TdxStringHelper.StartsWith(ATypeName, 'char(') then
      begin
        ASize := StrToInt(Copy(ATypeName, 6, Length(ATypeName) - 6));
        Exit(TdxDBColumnType.String);
      end;
      if TdxStringHelper.StartsWith(ATypeName, 'varchar(') then
      begin
        ASize := StrToInt(Copy(ATypeName, 9, Length(ATypeName) - 9));
        Exit(TdxDBColumnType.String);
      end;
      Result := TdxDBColumnType.Unknown;
    end;
  end;
end;

function TdxMySQLConnectionProvider.GetFieldValue(ADataSet: TDataSet; AFieldIndex: Integer): Variant;
var
  AField: TField;
begin
  AField := ADataSet.Fields[AFieldIndex];
  if AField.IsNull then
    Result := Null
  else
    Result := AField.Value;

end;

function TdxMySQLConnectionProvider.GetIsVersion5: Boolean;
begin
  if FIsVersion5 = bDefault then
    FIsVersion5 := dxBooleanToDefaultBoolean(not TdxStringHelper.StartsWith(GetMySqlServerVersion, '4.'));
  Result := FIsVersion5 = bTrue;
end;

class procedure TdxMySQLConnectionProvider.PopulateConnectionParameters;
begin
  FConnectionParameters.AddOrSetValue(TdxConnectionTypes.ADO, TConnectionParameters.Create(['Database']));
  FConnectionParameters.AddOrSetValue(TdxConnectionTypes.FireDAC, TConnectionParameters.Create(['Database']));
end;

class procedure TdxMySQLConnectionProvider.PopulateDBTypes;
begin
  FDBTypes.Add('bigint unsigned', TdxDBColumnType.UInt64);
  FDBTypes.Add('bigint', TdxDBColumnType.Int64);
  FDBTypes.Add('bit', TdxDBColumnType.Byte);
  FDBTypes.Add('blob', TdxDBColumnType.ByteArray);
  FDBTypes.Add('char(1)', TdxDBColumnType.Char);
  FDBTypes.Add('date', TdxDBColumnType.DateTime);
  FDBTypes.Add('datetime', TdxDBColumnType.DateTime);
  FDBTypes.Add('decimal', TdxDBColumnType.Decimal);
  FDBTypes.Add('double', TdxDBColumnType.Double);
  FDBTypes.Add('float', TdxDBColumnType.Single);
  FDBTypes.Add('int unsigned', TdxDBColumnType.UInt32);
  FDBTypes.Add('int', TdxDBColumnType.Int32);
  FDBTypes.Add('longblob', TdxDBColumnType.ByteArray);
  FDBTypes.Add('longtext', TdxDBColumnType.String);
  FDBTypes.Add('mediumblob', TdxDBColumnType.ByteArray);
  FDBTypes.Add('mediumtext', TdxDBColumnType.String);
  FDBTypes.Add('smallint unsigned', TdxDBColumnType.UInt16);
  FDBTypes.Add('smallint', TdxDBColumnType.Int16);
  FDBTypes.Add('text', TdxDBColumnType.String);
  FDBTypes.Add('tinyblob', TdxDBColumnType.ByteArray);
  FDBTypes.Add('tinyint unsigned', TdxDBColumnType.Byte);
  FDBTypes.Add('tinyint(1)', TdxDBColumnType.Boolean);
  FDBTypes.Add('tinyint', TdxDBColumnType.SByte);
end;

initialization
  TdxSQLConnectionProviderFactory.Register(TdxMySQLConnectionProvider);

finalization
  TdxSQLConnectionProviderFactory.Unregister(TdxMySQLConnectionProvider);
end.

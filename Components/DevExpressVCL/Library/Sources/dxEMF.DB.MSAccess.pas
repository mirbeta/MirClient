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

unit dxEMF.DB.MSAccess;

{$HPPEMIT '#pragma link "dxEMF.DB.MSAccess"'}

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, Generics.Collections, DB, Rtti,
  ADOInt, OleDB, ComObj,
  dxEMF.DB.Model,
  dxEMF.Types,
  dxEMF.DB.Criteria,
  dxEMF.DB.Query,
  dxEMF.DB.SQLConnectionProvider;

type

  { TdxAccessConnectionProvider }

  TdxAccessConnectionProvider = class(TdxSQLConnectionProvider)
  public const
    MaximumStringSize = 255;
    SortColumnsAlphabetically: Boolean = True;
  strict private
    class var
      FConnectionParameters: TDictionary<string, TdxConnectionParameterValues>;
      FDBTypes: TDictionary<string, TdxDBColumnType>;
    class destructor Destroy;
    class function GetConnectionParameters: TDictionary<string, TdxConnectionParameterValues>; static;
    class procedure PopulateConnectionParameters; static; inline;
    class procedure PopulateDBTypes; static; inline;
  protected
    function CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>; override;
    function ConvertToDBParameter(const AParameterValue: TValue): TValue; override;
    class function GetDBTypes: TDictionary<string, TdxDBColumnType>; override;
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
    function GetIdentity(ASql: TdxQuery): Int64; override;
    function GetSafeNameTableMaxLength: Integer; override;
    function GetSafeNameRoot(const AOriginalName: string): string; override;
    function GetMaximumStringSize: Integer; override;
    function NeedsIndexForForeignKey: Boolean; override;
    function ProcedureContainsParameters(const ASourceString: string): Boolean;
  public
    class function GetDBEngine: TdxDBEngine; override;

    procedure CreateDatabase(const ADatabaseName: string; const AValues: array of string); override;
    procedure CreateForeignKey(ATable: TdxDBTable; AForeignKey: TdxDBForeignKey); override;
    procedure CreateTable(ATable: TdxDBTable); override;
    function FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string; override;
    function FormatColumn(const AColumnName, ATableAlias: string): string; override;
    function FormatColumn(const AColumnName: string): string; override;
    function FormatConstraint(const AConstraintName: string): string; override;
    function FormatDelete(const ATableName, AWhereClause: string): string; override;
    function FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string; overload; override;
    function FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType;
      const AOperands: TArray<TValue>): string; overload; override;
    function FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string; override;
    function FormatInsertDefaultValues(ATable: TdxDBTable): string; override;
    function FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause: string;
      ATopSelectedRecords: Integer): string; override;
    function FormatTable(const ASchemaName, ATableName, ATableAlias: string): string; override;
    function FormatTable(const ASchemaName, ATableName: string): string; override;
    function FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string; override;
    function GetConnectionParameter(const AConnectionType: TdxConnectionType; AParameter: TdxConnectionParameter): string; override;
    function GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer; var ACreateParameter: Boolean): string; override;
    function GetSQLCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetStorageTableNames(AIncludeViews: Boolean): TArray<string>; override;
    procedure GetTableSchema(ATable: TdxDBTable; ACheckIndexes: Boolean; ACheckForeignKeys: Boolean); override;
    class property ConnectionParameters: TDictionary<string, TdxConnectionParameterValues> read GetConnectionParameters;
  end;

implementation

uses
  StrUtils, TypInfo, RegularExpressions, DateUtils,
  dxCore, dxStringHelper,
  dxEMF.Utils,
  dxEMF.DB.Utils,
  dxEMF.Strs, TimeSpan;

type

  { TdxAccessFormatterHelper }

  TdxAccessFormatterHelper = class sealed
  public
    class function GetSafeNameAccess(const AOriginalName: string): string; static;
    class function FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string; static;
    class function FormatColumn(const AColumnName: string): string; overload; static;
    class function FormatColumn(const AColumnName, ATableAlias: string): string; overload; static;
    class function FormatConstraint(const AConstraintName: string): string; static;
    class function FormatDelete(const ATableName, AWhereClause: string): string; static;
    class function FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string; overload; static;
    class function FnPadRight(const AOperands: TArray<string>): string; static;
    class function FnPadLeft(const AOperands: TArray<string>): string; static;
    class function FnCharIndex(const AOperands: TArray<string>): string; static;
    class function FnSubstring(const AOperands: TArray<string>): string; static;
    class function FnRemove(const AOperands: TArray<string>): string; static;
    class function FnConcat(const AOperands: TArray<string>): string; static;
    class function FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType;
      const AOperands: TArray<TValue>): string; overload; static;
    class function FnUtcNow(AProcessParameter: TdxProcessParameter): string; static;
    class function FormatInsert(const ATableName, AFields, AValues: string): string; static;
    class function FormatInsertDefaultValues(const ATableName: string): string; static;
    class function FormatTable(const ASchemaName, ATableName: string): string; overload; static;
    class function FormatTable(const ASchemaName, ATableName, ATableAlias: string): string; overload; static;
    class function FormatUpdate(const ATableName, ASets, AWhereClause: string): string; static;
  end;

{ TdxAccessFormatterHelper }

class function TdxAccessFormatterHelper.FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string;
begin
  case AOperatorType of
    TdxBinaryOperatorType.Modulo:
      Result := Format('%s mod %s', [ALeftOperand, ARightOperand]);
    TdxBinaryOperatorType.BitwiseXor,
    TdxBinaryOperatorType.BitwiseOr,
    TdxBinaryOperatorType.BitwiseAnd:
      raise ENotSupportedException.Create('Jet drivers (Microsoft ODBC Driver for Access and Microsoft OLE DB Provider for Jet) do not support bitwise operators.');
    else
      Result := TdxBaseFormatterHelper.DefaultFormatBinary(AOperatorType, ALeftOperand, ARightOperand);
  end;
end;

class function TdxAccessFormatterHelper.FormatColumn(const AColumnName: string): string;
begin
  Result := '[' + AColumnName + ']';
end;

class function TdxAccessFormatterHelper.FormatColumn(const AColumnName, ATableAlias: string): string;
begin
  Result := Format('%1:s.[%0:s]', [AColumnName, ATableAlias]);
end;

class function TdxAccessFormatterHelper.FormatConstraint(const AConstraintName: string): string;
begin
  Result := '[' + AConstraintName + ']';
end;

class function TdxAccessFormatterHelper.FormatDelete(const ATableName, AWhereClause: string): string;
begin
  Result := Format('delete from %s where %s', [ATableName, AWhereClause]);
end;

class function TdxAccessFormatterHelper.FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string;
var
  ASb: TStringBuilder;
  AIndex, ACounter: Integer;
begin
  case AOperatorType of
    TdxFunctionOperatorType.Abs:
      Result := Format('Abs(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sqr:
      Result := Format('Sqr(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Random:
      Result := 'Rnd()';
    TdxFunctionOperatorType.Log:
      case Length(AOperands) of
        1:
          Result := Format('Log(%s)', [AOperands[0]]);
        2:
          Result := Format('(Log(%s) / Log(%s))', [AOperands[0], AOperands[1]]);
        else
          Result := '';
      end;
    TdxFunctionOperatorType.Log10:
      Result := Format('(Log(%s) / Log(10))', [AOperands[0]]);
    TdxFunctionOperatorType.Sin:
      Result := Format('Sin(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Tan:
      Result := Format('Tan(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan:
      Result := Format('Atn(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan2:
      Result := Format('IIF((%0:s) = 0, IIF((%1:s) >= 0, 0, Atn(1) * 4), 2 * Atn(%0:s / (Sqr((%1:s) * (%1:s) + (%0:s) * (%0:s)) + (%1:s))))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Cos:
      Result := Format('Cos(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcCos:
      Result := Format('IIF((%0:s) = 1, 0, Atn(-(%0:s) / Sqr(-(%0:s) * (%0:s) + 1)) + Atn(1) * 2)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcSin:
      Result := Format('IIF((%0:s) = 1, Atn(1) * 2, Atn((%0:s) / Sqr(-(%0:s) * (%0:s) + 1)))', [AOperands[0]]);
    TdxFunctionOperatorType.Exp:
      Result := Format('Exp(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Cosh:
      Result := Format('((Exp(%0:s) + Exp(-(%0:s))) / 2)', [AOperands[0]]);
    TdxFunctionOperatorType.Sinh:
      Result := Format('((Exp(%0:s) - Exp(-(%0:s))) / 2)', [AOperands[0]]);
    TdxFunctionOperatorType.Tanh:
      Result := Format('((Exp(%0:s) - Exp(-(%0:s))) / (Exp(%0:s) + Exp(-(%0:s))))', [AOperands[0]]);
    TdxFunctionOperatorType.Power:
      Result := Format('((%0:s)^(%1:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Max:
      Result := Format('iif(%0:s > %1:s, %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Min:
      Result := Format('iif(%0:s < %1:s, %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IsNull:
      case Length(AOperands) of
        1:
          Result := Format('isnull(%s)', [AOperands[0]]);
        2:
          Result := Format('iif(isnull(%0:s), %1:s, %0:s)', [AOperands[0], AOperands[1]]);
        else
          Result := '';
      end;
    TdxFunctionOperatorType.IsNullOrEmpty:
      Result := Format('(isnull(%0:s) or len(%0:s) = 0)', [AOperands[0]]);
    TdxFunctionOperatorType.StartsWith:
      Result := Format('(Left(%0:s, Len(%1:s)) = (%1:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.EndsWith:
      Result := Format('(Right(%0:s, Len(%1:s)) = (%1:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Contains:
      Result := Format('(Instr(%0:s, %1:s) > 0)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Iif:
      begin
        if (Length(AOperands) < 3) or ((Length(AOperands) mod 2) = 0) then
          raise EArgumentException.Create(sdxFilteringTheIifFunctionOperatorRequiresThree);
        if Length(AOperands) = 3 then
          Result := Format('IIF(%0:s, %1:s, %2:s)', [AOperands[0], AOperands[1], AOperands[2]])
        else
        begin
          ASb := TStringBuilder.Create;
          try
            AIndex := -2;
            ACounter := 0;
            repeat
              Inc(AIndex, 2);
              ASb.AppendFormat('IIF(%s, %s, ', [AOperands[AIndex], AOperands[AIndex + 1]]);
              Inc(ACounter);
            until not ((AIndex + 3) < Length(AOperands));;
            ASb.Append(AOperands[AIndex + 2]);
            ASb.Append(')', ACounter);
            Result := ASb.ToString;
          finally
            ASb.Free;
          end;
        end;
      end;
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
      Result := Format('Sgn(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Floor:
      Result := Format('int(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Ceil:
      Result := Format('IIF(((%0:s) > 0), iif((%0:s) - fix(%0:s) = 0, %0:s, fix(%0:s) + 1), fix(%0:s))', [AOperands[0]]);
    TdxFunctionOperatorType.Trim:
      Result := Format('Trim(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Ascii:
      Result := Format('Asc(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Char:
      Result := Format('Chr(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ToInteger:
      Result := Format('Clng(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ToInt64:
      raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.ToSingle:
      Result := Format('CSng(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ToDouble:
      Result := Format('CDbl(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ToDecimal:
      Result := Format('CCur(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ToString:
      Result := Format('Str(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Reverse:
      raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.Concat:
      Result := FnConcat(AOperands);
    TdxFunctionOperatorType.UpperCase:
      Result := Format('UCase(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.LowerCase:
      Result := Format('LCase(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Insert:
      Result := Format('(LEFT(%0:s, %1:s) + (%2:s) + RIGHT(%0:s, len(%0:s) - (%1:s)))', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Remove:
      Result := FnRemove(AOperands);
    TdxFunctionOperatorType.Substring:
      Result := FnSubstring(AOperands);
    TdxFunctionOperatorType.CharIndex:
      Result := FnCharIndex(AOperands);
    TdxFunctionOperatorType.PadLeft:
      Result := FnPadLeft(AOperands);
    TdxFunctionOperatorType.PadRight:
      Result := FnPadRight(AOperands);
    TdxFunctionOperatorType.MillisecondOf:
      raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.SecondOf:
      Result := Format('DATEPART('#$27's'#$27', %s)', [AOperands[0]]);
    TdxFunctionOperatorType.MinuteOf:
      Result := Format('DATEPART('#$27'n'#$27', %s)', [AOperands[0]]);
    TdxFunctionOperatorType.HourOf:
      Result := Format('DATEPART('#$27'h'#$27', %s)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOf:
      Result := Format('DATEPART('#$27'd'#$27', %s)', [AOperands[0]]);
    TdxFunctionOperatorType.MonthOf:
      Result := Format('DATEPART('#$27'm'#$27', %s)', [AOperands[0]]);
    TdxFunctionOperatorType.YearOf:
      Result := Format('DATEPART('#$27'yyyy'#$27', %s)', [AOperands[0]]);
    TdxFunctionOperatorType.GetTimeOfDay:
      Result := Format('((DATEPART('#$27'h'#$27', %0:s)) * 36000000000) + ((DATEPART('#$27'n'#$27', %0:s)) * 600000000) + (DATEPART('#$27's'#$27', %0:s) * 10000000)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheWeek:
      Result := Format('(DatePart ('#$27'w'#$27', %0:s, 1, 0) - 1)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheYear:
      Result := Format('DatePart ('#$27'y'#$27', %s)', [AOperands[0]]);
    TdxFunctionOperatorType.DateOf:
      Result := Format('iif(isnull(%0:s), %0:s, DateSerial(Datepart('#$27'yyyy'#$27',%0:s), DATEPART('#$27'm'#$27', %0:s) , DATEPART('#$27'd'#$27', %0:s)))', [AOperands[0]]);
    TdxFunctionOperatorType.IncTick:
      Result := Format('DATEADD('#$27's'#$27', Fix((%1:s) / 10000000) mod 86400, %0:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMillisecond:
      Result := Format('DATEADD('#$27's'#$27', Fix((%1:s) / 1000) mod 86400, %0:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.AddTimeSpan,
    TdxFunctionOperatorType.IncSecond:
      Result := Format('DATEADD('#$27's'#$27', %1:s, %0:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMinute:
      Result := Format('DATEADD('#$27's'#$27', Fix((%1:s) * 60) mod 86400, DATEADD('#$27'd'#$27', Fix(((%1:s) * 60) / 86400), %0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncHour:
      Result := Format('DATEADD('#$27's'#$27', Fix((%1:s) * 3600) mod 86400, DATEADD('#$27'd'#$27', Fix(((%1:s) * 3600) / 86400), %0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncDay:
      Result := Format('DATEADD('#$27's'#$27', (Fix((%1:s) * 86400)) mod 86400, DATEADD('#$27'd'#$27', Fix(%1:s), %0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMonth:
      Result := Format('DATEADD('#$27'm'#$27', %1:s, %0:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncYear:
      Result := Format('DATEADD('#$27'yyyy'#$27', %1:s, %0:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.DaysBetween:
      Result := Format('DATEDIFF('#$27'd'#$27', %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.HoursBetween:
      Result := Format('DATEDIFF('#$27'h'#$27', %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MillisecondsBetween:
      Result := Format('(DATEDIFF('#$27's'#$27', %0:s, %1:s) * 1000)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MinutesBetween:
      Result := Format('DATEDIFF('#$27'n'#$27', %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MonthsBetween:
      Result := Format('DATEDIFF('#$27'm'#$27', %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.SecondsBetween:
      Result := Format('DATEDIFF('#$27's'#$27', %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.DateDiffTicks:
      Result := Format('(DATEDIFF('#$27's'#$27', %0:s, %1:s) * 10000000)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.YearsBetween:
      Result := Format('DATEDIFF('#$27'yyyy'#$27', %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Now:
      Result := 'NOW()';
    TdxFunctionOperatorType.Today:
      Result := 'DATE()';
    TdxFunctionOperatorType.BigMul,
    TdxFunctionOperatorType.Replace:
      raise ENotSupportedException.Create('');
    else
      Result := '';
  end;
end;

class function TdxAccessFormatterHelper.FnPadRight(const AOperands: TArray<string>): string;
begin
  if Length(AOperands) = 2 then
    Result := Format('IIF(((%1:s) - Len(%0:s)) > 0, (%0:s) + Space(%1:s - Len(%0:s)), %0:s)', [AOperands[0], AOperands[1]])
  else
    raise ENotSupportedException.Create('');
end;

class function TdxAccessFormatterHelper.FnPadLeft(const AOperands: TArray<string>): string;
begin
  if Length(AOperands) = 2 then
    Result := Format('IIF(((%1:s) - Len(%0:s)) > 0, Space((%1:s) - Len(%0:s)) +  (%0:s), %0:s)', [AOperands[0], AOperands[1]])
  else
    raise ENotSupportedException.Create('');
end;

class function TdxAccessFormatterHelper.FnCharIndex(const AOperands: TArray<string>): string;
begin
  case Length(AOperands) of
    2:
      Result := Format('(Instr(%1:s, %0:s) - 1)', [AOperands[0], AOperands[1]]);
    3:
      Result := Format('IIF(Instr(Right(%1:s, Len(%1:s) - (%2:s)), %0:s) = 0 , -1, Instr(Right(%1:s, Len(%1:s) - (%2:s)), %0:s) + (%2:s) - 1)',
        [AOperands[0], AOperands[1], AOperands[2]]);
    4:
      Result := Format('IIF(Instr(Left(Right(%1:s, Len(%1:s) - (%2:s)), %3:s), %0:s) = 0 , -1, Instr(Left(Right(%1:s, Len(%1:s) - (%2:s)), %3:s), %0:s) + (%2:s) - 1)',
        [AOperands[0], AOperands[1], AOperands[2], AOperands[3]]);
    else
      raise ENotSupportedException.Create('');
  end;
end;

class function TdxAccessFormatterHelper.FnSubstring(const AOperands: TArray<string>): string;
begin
  case Length(AOperands) of
    2:
      Result := Format('Mid(%s, (%s) + 1)', [AOperands[0], AOperands[1]]);
    3:
      Result := Format('Mid(%s, (%s) + 1, %s)', [AOperands[0], AOperands[1], AOperands[2]]);
    else
      raise ENotSupportedException.Create('');
  end;
end;

class function TdxAccessFormatterHelper.FnRemove(const AOperands: TArray<string>): string;
begin
  case Length(AOperands) of
    2:
      Result := Format('LEFT(%s, %s)', [AOperands[0], AOperands[1]]);
    3:
      Result := Format('(LEFT(%0:s, %1:s) + RIGHT(%0:s, len(%0:s) - (%1:s) - (%2:s)))', [AOperands[0], AOperands[1], AOperands[2]]);
    else
      raise ENotSupportedException.Create('');
  end;
end;

class function TdxAccessFormatterHelper.FnConcat(const AOperands: TArray<string>): string;
begin
  Result := TdxFormatterHelper.Concat(AOperands, ' & ');
end;

class function TdxAccessFormatterHelper.FormatFunction(AProcessParameter: TdxProcessParameter;
  AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string;
begin
  case AOperatorType of
    TdxFunctionOperatorType.UtcNow:
      Result := FnUtcNow(AProcessParameter);
    else
      Result := '';
  end;
end;

class function TdxAccessFormatterHelper.FnUtcNow(AProcessParameter: TdxProcessParameter): string;
var
  ANow, AUtcNow: TDateTime;
  ADiffHour: Integer;
  ACriteriaOperator: IdxCriteriaOperator;
  S: string;
begin
  ANow := Now;
  AUtcNow := TTimeZone.Local.ToUniversalTime(ANow);
  ADiffHour := HoursBetween(AUtcNow, ANow);
  ACriteriaOperator := TdxOperandValue.Create(ADiffHour);
  if Assigned(AProcessParameter) then
    S := AProcessParameter(ACriteriaOperator)
  else
    S := IntToStr(ADiffHour);
  Result := Format('DATEADD('#$27'h'#$27', %s, NOW())', [S]);
end;

class function TdxAccessFormatterHelper.FormatInsert(const ATableName, AFields, AValues: string): string;
begin
  Result := Format('insert into %s(%s)values(%s)', [ATableName, AFields, AValues]);
end;

class function TdxAccessFormatterHelper.FormatInsertDefaultValues(const ATableName: string): string;
begin
  Result := Format('insert into %s default values', [ATableName]);
end;

class function TdxAccessFormatterHelper.FormatTable(const ASchemaName, ATableName: string): string;
begin
  Result := '[' + ATableName + ']';
end;

class function TdxAccessFormatterHelper.FormatTable(const ASchemaName, ATableName, ATableAlias: string): string;
begin
  Result := Format('[%s] %s', [ATableName, ATableAlias]);
end;

class function TdxAccessFormatterHelper.FormatUpdate(const ATableName, ASets, AWhereClause: string): string;
begin
  Result := Format('update %s set %s where %s', [ATableName, ASets, AWhereClause]);
end;

class function TdxAccessFormatterHelper.GetSafeNameAccess(const AOriginalName: string): string;
var
  AResult: string;
  ALength, I: Integer;
  ASpacesToUnderscore: Boolean;
begin
  if AOriginalName = '' then
    Exit(AOriginalName);
  AResult := AOriginalName;
  ALength := Length(AResult);
  ASpacesToUnderscore := (AResult[1] = ' ') or (AResult[ALength] = ' ');
  for I := 1 to ALength do
  begin
    case AResult[I] of
      #0..#31,
      '[',
      ']',
      '!',
      '.',
      '`':
        AResult[I] := '_';
      ' ':
        if ASpacesToUnderscore then
          AResult[I] := '_';
    end;
  end;
  Result := AResult;
end;

{ TdxAccessConnectionProvider }

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForBoolean(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'bit';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'byte';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForSByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'short';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForChar(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'char(1)';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForDecimal(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'currency';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForDouble(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'double';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForSingle(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'single';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'int';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForUInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'decimal(10,0)';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'short';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForUInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'int';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'decimal(20,0)';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForUInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'decimal(20,0)';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForString(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  if (AColumn.Size > 0) and (AColumn.Size <= MaximumStringSize) then
    Result := 'varchar(' + IntToStr(AColumn.Size) + ')'
  else
    if AColumn.Size = 0 then
      Result := 'varchar(' + IntToStr(MaximumStringSize) + ')'
    else
      Result := 'LONGTEXT';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForDateTime(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'datetime';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForGuid(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'guid';
end;

function TdxAccessConnectionProvider.GetSQLCreateColumnTypeForByteArray(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'longbinary';
end;

function TdxAccessConnectionProvider.GetSqlCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := GetSQLCreateColumnType(ATable, AColumn);
  if AColumn.ColumnType = TdxDBColumnType.Boolean then
    Exit;
  if AColumn.IsKey or not AColumn.IsNullable then
    Result := Result + ' NOT NULL'
  else
    Result := Result + ' NULL';
  if AColumn.IsKey then
    if AColumn.IsIdentity and
      (AColumn.ColumnType in [TdxDBColumnType.Int32, TdxDBColumnType.Int64]) and IsSingleColumnPKColumn(ATable, AColumn) then
    begin
      if AColumn.ColumnType = TdxDBColumnType.Int64 then
        raise ENotSupportedException.CreateFmt(sdxAutoIncrementedKeyNotSupported,
          [TdxFormatterHelper.GetName(AColumn.ColumnType), ClassName]);
      Result := Result + ' IDENTITY';
    end;
end;

function TdxAccessConnectionProvider.CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>;
var
  AList: TList<TdxDBTable>;
  ATable: TdxDBTable;
  AName: string;
  ADataSetTable, ADataSetView, ADataSetProc: TDataSet;
begin
  AList := TList<TdxDBTable>.Create;
  try
    for ATable in ATables do
    begin
      AName := ComposeSafeTableName(ATable.Name);
      ADataSetTable := ConnectionVendor.GetSchema(TdxSchemaInfo.Tables, '', '', AName);
      try
        ADataSetView := ConnectionVendor.GetSchema(TdxSchemaInfo.Views, '', '', AName);
        try
          ADataSetProc := ConnectionVendor.GetSchema(TdxSchemaInfo.Views, '', '', AName);
          try
            if ADataSetTable.IsEmpty and ADataSetView.IsEmpty and ADataSetProc.IsEmpty then
              AList.Add(ATable)
            else
            begin
              if not ADataSetView.IsEmpty then
                ATable.IsView := True
              else
                if not ADataSetProc.IsEmpty then
                  ATable.IsView := not ProcedureContainsParameters(ADataSetProc.FieldByName('PROCEDURE_DEFINITION').AsString);
            end;
          finally
            ADataSetProc.Free
          end;
        finally
          ADataSetView.Free;
        end;
      finally
        ADataSetTable.Free;
      end;
    end;
    Result := AList.ToArray;
  finally
    AList.Free;
  end;
end;

function TdxAccessConnectionProvider.ConvertToDBParameter(const AParameterValue: TValue): TValue;
var
  S: string;
  ABytes: TBytes;
begin
  if AParameterValue.IsDateTime then
    Result := DateTimeToStr(AParameterValue.AsDateTime)
  else
    if AParameterValue.TypeInfo = TypeInfo(string) then
    begin
      S := AParameterValue.AsString;
      if Length(S) > MaximumStringSize then
      begin
        ABytes := TEncoding.Unicode.GetBytes(S);
        Result := TValue.From<TBytes>(ABytes);
      end
      else
        Result := AParameterValue
    end
    else
      Result := inherited ConvertToDBParameter(AParameterValue);
end;

function TdxAccessConnectionProvider.GetConnectionParameter(const AConnectionType: TdxConnectionType;
  AParameter: TdxConnectionParameter): string;
begin
  Result := ConnectionParameters[AConnectionType][AParameter];
end;

class function TdxAccessConnectionProvider.GetConnectionParameters: TDictionary<string, TdxConnectionParameterValues>;
begin
  if FConnectionParameters = nil then
  begin
    FConnectionParameters := TDictionary<string, TdxConnectionParameterValues>.Create;
    PopulateConnectionParameters;
  end;
  Result := FConnectionParameters;
end;

class function TdxAccessConnectionProvider.GetDBEngine: TdxDBEngine;
begin
  Result := TdxDBEngines.MSAccess;
end;

class function TdxAccessConnectionProvider.GetDBTypes: TDictionary<string, TdxDBColumnType>;
begin
  if FDBTypes = nil then
  begin
    FDBTypes := TDictionary<string, TdxDBColumnType>.Create;
    PopulateDBTypes;
  end;
  Result := FDBTypes;
end;

function TdxAccessConnectionProvider.GetIdentity(ASql: TdxQuery): Int64;
begin
  Execute(ASQL);
  Result := ConnectionVendor.ExecuteScalar('select @@Identity');
end;

function TdxAccessConnectionProvider.GetMaximumStringSize: Integer;
begin
  Result := MaximumStringSize;
end;

procedure TdxAccessConnectionProvider.CreateDatabase(const ADatabaseName: string; const AValues: array of string);
var
  ACatalog: OleVariant;
begin
  ACatalog := CreateOleObject('ADOX.Catalog');
  ACatalog.Create('Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' + ADatabaseName + ';');
end;

function TdxAccessConnectionProvider.GetSafeNameTableMaxLength: Integer;
begin
  Result := 64;
end;

function TdxAccessConnectionProvider.GetSafeNameRoot(const AOriginalName: string): string;
begin
  Result := TdxAccessFormatterHelper.GetSafeNameAccess(AOriginalName);
end;

procedure TdxAccessConnectionProvider.CreateForeignKey(ATable: TdxDBTable; AForeignKey: TdxDBForeignKey);
begin
  if AForeignKey.PrimaryKeyTable = 'XPObjectType' then
    Exit;
  inherited CreateForeignKey(ATable, AForeignKey);
end;

procedure TdxAccessConnectionProvider.CreateTable(ATable: TdxDBTable);
var
  AColumns: TStringBuilder;
  AColumn: TdxDBColumn;
  I: Integer;
begin
  AColumns := TStringBuilder.Create;
  try
    for AColumn in ATable.Columns do
    begin
      if AColumns.Length > 0 then
        AColumns.Append(', ');
      AColumns.Append(FormatColumnSafe(AColumn.Name)).
        Append(' ').
        Append(GetSQLCreateColumnFullAttributes(ATable, AColumn));
    end;
    if ATable.HasPrimaryKey and not ATable.GetColumn(ATable.PrimaryKey.Columns[0]).IsIdentity then
    begin
      AColumns.Append(', primary key (');
      for I := 0 to ATable.PrimaryKey.Columns.Count - 1 do
      begin
        if I > 0 then
          AColumns.Append(', ');
        AColumns.Append(FormatColumnSafe(ATable.PrimaryKey.Columns[I]));
      end;
      AColumns.Append(')');
    end;
    ExecuteSQLSchemaUpdate('Table', ATable.Name, '', FormatCreateTable(FormatTableSafe(ATable), AColumns.ToString));
  finally
    AColumns.Free;
  end;
end;

class destructor TdxAccessConnectionProvider.Destroy;
begin
  FreeAndNil(FConnectionParameters);
  FreeAndNil(FDBTypes);
end;

function TdxAccessConnectionProvider.FormatTable(const ASchemaName, ATableName: string): string;
begin
  Result := TdxAccessFormatterHelper.FormatTable(ASchemaName, ATableName);
end;

function TdxAccessConnectionProvider.FormatTable(const ASchemaName, ATableName, ATableAlias: string): string;
begin
  Result := TdxAccessFormatterHelper.FormatTable(ASchemaName, ATableName, ATableAlias);
end;

function TdxAccessConnectionProvider.FormatColumn(const AColumnName: string): string;
begin
  Result := TdxAccessFormatterHelper.FormatColumn(AColumnName);
end;

function TdxAccessConnectionProvider.FormatColumn(const AColumnName, ATableAlias: string): string;
begin
  Result := TdxAccessFormatterHelper.FormatColumn(AColumnName, ATableAlias);
end;

function TdxAccessConnectionProvider.FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause,
  AGroupByClause, AHavingClause: string; ATopSelectedRecords: Integer): string;
var
  AModificatorsClause, AExpandedWhereClause, AExpandedGroupByClause, AExpandedHavingClause, AExpandedOrderByClause: string;
begin
  if ATopSelectedRecords <> 0 then
    AModificatorsClause := 'top ' + IntToStr(ATopSelectedRecords)
  else
    AModificatorsClause := '';
  if AWhereClause = '' then
    AExpandedWhereClause := ''
  else
    AExpandedWhereClause := #10'where ' + AWhereClause;
  if AGroupByClause <> '' then
    AExpandedGroupByClause := #10'group by ' + AGroupByClause
  else
    AExpandedGroupByClause := '';
  if AHavingClause <> '' then
    AExpandedHavingClause := #10'having ' + AHavingClause
  else
    AExpandedHavingClause := '';
  if AOrderByClause <> '' then
    AExpandedOrderByClause := #10'order by ' + AOrderByClause
  else
    AExpandedOrderByClause := '';
  Result := Format('select %s %s from %s%s%s%s%s',
    [AModificatorsClause, ASelectClause, AFromClause, AExpandedWhereClause, AExpandedGroupByClause, AExpandedHavingClause, AExpandedOrderByClause]);
end;

function TdxAccessConnectionProvider.FormatInsertDefaultValues(ATable: TdxDBTable): string;
begin
  Result := TdxAccessFormatterHelper.FormatInsertDefaultValues(FormatTableSafe(ATable));
end;

function TdxAccessConnectionProvider.FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string;
begin
  Result := TdxAccessFormatterHelper.FormatInsert(ATableName, AFieldNames, AValuesClause);
end;

function TdxAccessConnectionProvider.FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string;
begin
  Result := TdxAccessFormatterHelper.FormatUpdate(ATableName, ASetClause, AWhereClause);
end;

function TdxAccessConnectionProvider.FormatDelete(const ATableName, AWhereClause: string): string;
begin
  Result := TdxAccessFormatterHelper.FormatDelete(ATableName, AWhereClause);
end;

function TdxAccessConnectionProvider.FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand: string; const ARightOperand: string): string;
begin
  Result := TdxAccessFormatterHelper.FormatBinary(AOperatorType, ALeftOperand, ARightOperand);
end;

function TdxAccessConnectionProvider.FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string;
begin
  Result := TdxAccessFormatterHelper.FormatFunction(AOperatorType, AOperands);
  if Result = '' then
    Result := inherited FormatFunction(AOperatorType, AOperands);
end;

function TdxAccessConnectionProvider.FormatFunction(AProcessParameter: TdxProcessParameter;
  AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string;
begin
  Result := TdxAccessFormatterHelper.FormatFunction(AProcessParameter, AOperatorType, AOperands);
  if Result = '' then
    Result := inherited FormatFunction(AProcessParameter, AOperatorType, AOperands);
end;

function TdxAccessConnectionProvider.GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer;
  var ACreateParameter: Boolean): string;
begin
  Result := GetParamAlias(AIndex);
end;

function TdxAccessConnectionProvider.FormatConstraint(const AConstraintName: string): string;
begin
  Result := TdxAccessFormatterHelper.FormatConstraint(AConstraintName);
end;


function TdxAccessConnectionProvider.GetStorageTableNames(AIncludeViews: Boolean): TArray<string>;
var
  AResult: TList<string>;
  ATables, AViews, AProcedures: TDataSet;
begin
  AResult := TList<string>.Create;
  try
    ATables := ConnectionVendor.GetSchema(TdxSchemaInfo.Tables, '', '', '');
    try
      while not ATables.Eof do
      begin
        AResult.Add(ATables.FieldByName('TABLE_NAME').AsString);
        ATables.Next;
      end;
    finally
      ATables.Free;
    end;
    if AIncludeViews then
    begin
      AViews := ConnectionVendor.GetSchema(TdxSchemaInfo.Views, '', '', '');
      try
        while not AViews.Eof do
        begin
          AResult.Add(AViews.FieldByName('TABLE_NAME').AsString);
          AViews.Next;
        end;
      finally
        AViews.Free;
      end;
      AProcedures := ConnectionVendor.GetSchema(TdxSchemaInfo.Procedures, '', '', '');
      try
        while not AProcedures.Eof do
        begin
          if not ProcedureContainsParameters(AProcedures.FieldByName('PROCEDURE_DEFINITION').AsString) then
            AResult.Add(AProcedures.FieldByName('PROCEDURE_NAME').AsString);
          AProcedures.Next;
        end;
      finally
        AProcedures.Free;
      end;
    end;
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

procedure TdxAccessConnectionProvider.GetTableSchema(ATable: TdxDBTable; ACheckIndexes, ACheckForeignKeys: Boolean);
begin
  ConnectionVendor.GetTableSchema(ATable, ACheckIndexes, ACheckForeignKeys);
  if not SortColumnsAlphabetically then
    ATable.Columns.Sort(TdxDBColumnComparer.Create);
end;

function TdxAccessConnectionProvider.NeedsIndexForForeignKey: Boolean;
begin
  Result := False;
end;

class procedure TdxAccessConnectionProvider.PopulateConnectionParameters;
begin
  FConnectionParameters.AddOrSetValue(TdxConnectionTypes.ADO, TdxSQLConnectionProvider.TConnectionParameters.Create(['Data Source']));
  FConnectionParameters.AddOrSetValue(TdxConnectionTypes.FireDAC, TdxSQLConnectionProvider.TConnectionParameters.Create(['Database']));
end;

class procedure TdxAccessConnectionProvider.PopulateDBTypes;
begin
  FDBTypes.Add('decimal', TdxDBColumnType.Int64);
  FDBTypes.Add('counter', TdxDBColumnType.Int32);
  FDBTypes.Add('integer', TdxDBColumnType.Int32);
  FDBTypes.Add('int', TdxDBColumnType.Int32);
  FDBTypes.Add('bit', TdxDBColumnType.Boolean);
  FDBTypes.Add('byte', TdxDBColumnType.Byte);
  FDBTypes.Add('date', TdxDBColumnType.DateTime);
  FDBTypes.Add('time', TdxDBColumnType.DateTime);
  FDBTypes.Add('datetime', TdxDBColumnType.DateTime);
  FDBTypes.Add('money', TdxDBColumnType.Decimal);
  FDBTypes.Add('double', TdxDBColumnType.Double);
  FDBTypes.Add('float', TdxDBColumnType.Double);
  FDBTypes.Add('single', TdxDBColumnType.Single);
  FDBTypes.Add('real', TdxDBColumnType.Single);
  FDBTypes.Add('short', TdxDBColumnType.Int16);
  FDBTypes.Add('image', TdxDBColumnType.ByteArray);
  FDBTypes.Add('longbinary', TdxDBColumnType.ByteArray);
  FDBTypes.Add('binary', TdxDBColumnType.ByteArray);
  FDBTypes.Add('varbinary', TdxDBColumnType.ByteArray);
  FDBTypes.Add('general', TdxDBColumnType.ByteArray);
  FDBTypes.Add('varchar', TdxDBColumnType.String);
  FDBTypes.Add('longtext', TdxDBColumnType.String);
  FDBTypes.Add('longchar', TdxDBColumnType.String);
  FDBTypes.Add('ntext', TdxDBColumnType.String);
  FDBTypes.Add('memo', TdxDBColumnType.String);
  FDBTypes.Add('char', TdxDBColumnType.String);
  FDBTypes.Add('nchar', TdxDBColumnType.String);
end;

function TdxAccessConnectionProvider.ProcedureContainsParameters(const ASourceString: string): Boolean;
begin
  Result := {$IFDEF DELPHIXE3}(ASourceString.StartsWith('PARA')){$ELSE}TdxStringHelper.StartsWith(ASourceString, 'PARA'){$ENDIF} or
    (TRegex.Match(ASourceString, 'Forms]?!', [TRegexOption.roIgnoreCase]).Success);
end;



initialization
  TdxSQLConnectionProviderFactory.Register(TdxAccessConnectionProvider);

finalization
  TdxSQLConnectionProviderFactory.Unregister(TdxAccessConnectionProvider);

end.

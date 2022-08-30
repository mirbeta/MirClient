{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxServerModeSQLAdapters;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, DB, dxServerModeData, cxFilter;

type
  { TdxServerModeMSSQLAdapter }

  TdxServerModeMSSQLAdapter = class(TdxServerModeCustomSQLAdapter)
  protected
    function CanIdentifyInsertingRow: Boolean; override;
    function GetCastAsDateTimeFormat: string; override;
    function GetCastAsTimeFormat: string; override;
    function GetInsertingRowOutKey(const AKey: string): string; override;
    function GetInsertSQLString(const AFrom, AOutRowKey, AFields, AValues: string): string; override;
    function GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
      ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string; override;
    function GetUpdateSQLString(const AFrom, AOutRowKey, AValues, AWhere: string): string; override;
    function MakeEscapedValue(const AValue: string; AEscapePercentWildcard, AEscapeUnderscoreWildcard: Boolean): string; override;
  public
    class function GetDisplayName: string; override;
    function GetSQLQueryKeyFieldNames(const ATableName: string): string; override;
    function GetSQLQuerySchemaNames: string; override;
  end;

  { TdxServerModeMySQLAdapter }

  TdxServerModeMySQLAdapter = class(TdxServerModeCustomSQLAdapter)
  protected
    procedure CheckParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    procedure CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    function GetEscapeTextForParamName: string; override;
    function GetParamCheck: Boolean; override;
    procedure RenameParams(AParams: TdxServerModeParams; var ACommandText: string); override;

    function GetCastAsDateTimeFormat: string; override;
    function GetCastAsTimeFormat: string; override;
    function GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
      ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string; override;
    function GetQuoteChar: string; override;
    function QuotedString(const AParamValue: string): string; override;

    function DateDec(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; override;
    function DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string; override;
    function DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; override;
    function DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string; override;
    function IsMicrosecondSupported: Boolean; override;
    function IsMillisecondSupported: Boolean; override;
    function NeedCastGroupingByDateRange: Boolean; override;

    function GetCaseFullFormat: string; override;
  public
    class function GetDisplayName: string; override;
    function GetSQLQueryKeyFieldNames(const ATableName: string): string; override;
  end;

  { TdxServerModeFirebirdAdapter }

  TdxServerModeFirebirdAdapter = class(TdxServerModeCustomSQLAdapter)
  protected
    function CanUseParam(AParam: TdxServerModeParam): Boolean; override;
    procedure CheckParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    procedure CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    function GetParamCheck: Boolean; override;
    procedure RenameParams(AParams: TdxServerModeParams; var ACommandText: string); override;

    function CanIdentifyInsertingRow: Boolean; override;
    function CanUseResultFieldName: Boolean; override;
    function DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string; override;
    function GetCastAsDateTimeFormat: string; override;
    function GetCastAsTimeFormat: string; override;
    function GetDateTimeFormat: string; override;
    function GetInsertSQLString(const AFrom, AOutRowKey, AFields, AValues: string): string; override;
    function GetUpdateSQLString(const AFrom, AOutRowKey, AValues, AWhere: string): string; override;

    function GetFieldsRetrieveQueryFormat: string; override;
    function GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
      ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string; override;
  public
    class function GetDisplayName: string; override;
    function GetSQLQueryKeyFieldNames(const ATableName: string): string; override;
  end;

  { TdxServerModeInterBaseAdapter }

  TdxServerModeInterBaseAdapter = class(TdxServerModeFirebirdAdapter)
  protected
    function CanUseParam(AParam: TdxServerModeParam): Boolean; override;
    function CanUseResultFieldName: Boolean; override;
    function DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string; override;
    function GetCastAsDateTimeFormat: string; override;
    function GetCastAsTimeFormat: string; override;
    function IsDateRangeGroupingSupported: Boolean; override;

    function GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
      ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string; override;
    function GetTableNameFormat: string; override;
  public
    class function GetDisplayName: string; override;
  end;

  { TdxServerModeOracleAdapter }

  TdxServerModeOracleAdapter = class(TdxServerModeCustomSQLAdapter)
  private
    FNullsSortOrder: TdxNullsSortOrder;
    procedure SetNullsSortOrder(const Value: TdxNullsSortOrder);
  protected
    procedure CheckParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    procedure CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    function GetOrderFormat(AIsDesc: Boolean): string; override;
    function GetParamCheck: Boolean; override;
    function GetNullsSortOrder: TdxNullsSortOrder; override;
    procedure RenameParams(AParams: TdxServerModeParams; var ACommandText: string); override;

    function CanUseGroupingByDateRangeParams: Boolean; override;
    function DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string; override;
    function DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; override;
    function DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string; override;
    function GetCastAsDateTimeFormat: string; override;
    function GetCastAsTimeFormat: string; override;
    function GetDateTimeFormat: string; override;
    function TruncGroupingDate(const ADate: string; ADateTimeGrouping: TdxDateTimeGrouping): string; override;

    function GetSchemaName: string;
    function GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
      ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string; override;
  public
    constructor Create(ADataSource: TdxServerModeCustomDataSource); override;
    procedure Assign(Source: TPersistent); override;
    class function GetDisplayName: string; override;
    function GetSQLQueryKeyFieldNames(const ATableName: string): string; override;
    function GetSQLQuerySchemaNames: string; override;
  published
    property NullsSortOrder: TdxNullsSortOrder read FNullsSortOrder write SetNullsSortOrder default nsoFirstIfDesc;
  end;

  { TdxServerModePostgreSQLAdapter }

  TdxServerModePostgreSQLAdapter = class(TdxServerModeCustomSQLAdapter)
  private
    FNullsSortOrder: TdxNullsSortOrder;
    procedure SetNullsSortOrder(const Value: TdxNullsSortOrder);
  protected
    function CanUseResultFieldName: Boolean; override;
    function CanUseGroupingByDateRangeParams: Boolean; override;
    procedure CheckParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    procedure CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    function GetOrderFormat(AIsDesc: Boolean): string; override;
    function GetParamCheck: Boolean; override;
    function GetNullsSortOrder: TdxNullsSortOrder; override;
    procedure RenameParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    function QuotedString(const AParamValue: string): string; override;

    function CanIdentifyInsertingRow: Boolean; override;
    function DateDec(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; override;
    function DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string; override;
    function DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; override;
    function DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string; override;
    function GetCastAsDateTimeFormat: string; override;
    function GetCastAsTimeFormat: string; override;
    function GetEscapeTextForParamName: string; override;
    function GetInsertSQLString(const AFrom, AOutRowKey, AFields, AValues: string): string; override;
    function GetUpdateSQLString(const AFrom, AOutRowKey, AValues, AWhere: string): string; override;
    function TruncGroupingDate(const ADate: string; ADateTimeGrouping: TdxDateTimeGrouping): string; override;
    procedure CheckFilterFieldCaption(var AFieldCaption: string; AOperatorKind: TcxFilterOperatorKind); override;

    function GetSchemaName: string;
    function GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
      ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string; override;
  public
    constructor Create(ADataSource: TdxServerModeCustomDataSource); override;
    procedure Assign(Source: TPersistent); override;
    class function GetDisplayName: string; override;
    function GetSQLQueryKeyFieldNames(const ATableName: string): string; override;
  published
    property NullsSortOrder: TdxNullsSortOrder read FNullsSortOrder write SetNullsSortOrder default nsoFirstIfDesc;
  end;

  { TdxServerModeAdvantageAdapter }

  TdxServerModeAdvantageAdapter = class(TdxServerModeCustomSQLAdapter)
  protected
    function IsMillisecondSupported: Boolean; override;

    function DateDec(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; override;
    function DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string; override;
    function DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; override;
    function DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string; override;
    function GetCastAsDateFormat: string; override;
    function GetCastAsDateTimeFormat: string; override;
    function GetCastAsTimeFormat: string; override;
    function GetDatePart(APart: TdxSQLDatePart): string; override;
    function GetEscapeTextForParamName: string; override;
    procedure CheckFilterFieldCaption(var AFieldCaption: string; AOperatorKind: TcxFilterOperatorKind); override;

    function CanUseResultFieldName: Boolean; override;
    function CanUseGroupingByDateRangeParams: Boolean; override;
    procedure CheckParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    procedure CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    function GetParamCheck: Boolean; override;
    procedure RenameParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    function GetDateTimeFormat: string; override;

    function GetFieldsRetrieveQueryFormat: string; override;
    function GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
      ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string; override;
  public
    class function GetDisplayName: string; override;
    function GetSQLQueryKeyFieldNames(const ATableName: string): string; override;
  end;

  { TdxServerModeMSAccessAdapter }

  TdxServerModeMSAccessAdapter = class(TdxServerModeCustomSQLAdapter)
  protected
    procedure CheckParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    procedure CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    function GetParamCheck: Boolean; override;
    procedure RenameParams(AParams: TdxServerModeParams; var ACommandText: string); override;

    function IsMillisecondSupported: Boolean; override;
    function IsSkipClauseSupported: Boolean; override;

    function DateDec(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; override;
    function DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string; override;
    function DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string; override;
    function DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string; override;
    function DateTimeToString(const ADateTime: TDateTime): string; override;
    function GetCaseFullFormat: string; override;
    function GetCastAsDateTimeFormat: string; override;
    function GetCastAsTimeFormat: string; override;
    function GetDatePart(APart: TdxSQLDatePart): string; override;
    function IsNull(const AExpression: string): string; override;

    function MakeEscapedValue(const AValue: string; AEscapePercentWildcard, AEscapeUnderscoreWildcard: Boolean): string; override;
    function NeedEscapeFakeParamName(AParam: TdxServerModeParam): Boolean; override;

    function CanUseParam(AParam: TdxServerModeParam): Boolean; override;
    function CanUseGroupingByDateRangeParams: Boolean; override;
    function GetDateTimeFormat: string; override;
    function GetFieldNameFormat: string; override;
    function GetQuoteChar: string; override;
    function GetTableNameFormat: string; override;

    function GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
      ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string; override;
  public
    class function GetDisplayName: string; override;
  end;

  { TdxServerModeSQLiteAdapter }

  TdxServerModeSQLiteAdapter = class(TdxServerModeCustomSQLAdapter)
  protected
    procedure CheckParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    procedure CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string); override;
    function GetParamCheck: Boolean; override;
    procedure RenameParams(AParams: TdxServerModeParams; var ACommandText: string); override;

    function CanUseResultFieldName: Boolean; override;
    function GetDateTimeFormat: string; override;
    function IsDateRangeGroupingSupported: Boolean; override;

    function GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
      ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string; override;
  public
    class function GetDisplayName: string; override;
  end;

implementation

uses
  ConvUtils, StrUtils, cxClasses, dxCore, Variants, cxVariants;

type
  TdxServerModeCustomDataSourceAccess = class(TdxServerModeCustomDataSource);

{ TdxServerModeMSSQLAdapter }

class function TdxServerModeMSSQLAdapter.GetDisplayName: string;
begin
  Result := 'MSSQL Adapter';
end;

function TdxServerModeMSSQLAdapter.GetSQLQueryKeyFieldNames(const ATableName: string): string;
begin
  Result := Format('select c.COLUMN_NAME, COLUMNPROPERTY(OBJECT_ID(c.TABLE_SCHEMA + ''.'' + c.TABLE_NAME), c.COLUMN_NAME, ''IsIdentity'') ' +
    'FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE c join INFORMATION_SCHEMA.TABLE_CONSTRAINTS p on p.CONSTRAINT_NAME = c.CONSTRAINT_NAME ' +
    'WHERE c.TABLE_NAME = ''%s'' and p.CONSTRAINT_TYPE = ''PRIMARY KEY'' ' +
    'ORDER BY c.ORDINAL_POSITION asc', [ATableName]);
end;

function TdxServerModeMSSQLAdapter.GetSQLQuerySchemaNames: string;
begin
  Result := 'SELECT DISTINCT SCHEMA_NAME(schema_id) FROM sys.objects ORDER BY 1';
end;

function TdxServerModeMSSQLAdapter.CanIdentifyInsertingRow: Boolean;
begin
  Result := True;
end;

function TdxServerModeMSSQLAdapter.GetCastAsDateTimeFormat: string;
begin
  Result := 'CAST(%s as DATETIME)';
end;

function TdxServerModeMSSQLAdapter.GetCastAsTimeFormat: string;
begin
  Result := 'CAST(%s as TIME)';
end;

function TdxServerModeMSSQLAdapter.GetInsertingRowOutKey(const AKey: string): string;
begin
  Result := Format('INSERTED.%s', [AKey]);
end;

function TdxServerModeMSSQLAdapter.GetInsertSQLString(const AFrom, AOutRowKey, AFields, AValues: string): string;
begin
  Result := Format('INSERT INTO %s (%s) OUTPUT %s VALUES (%s)', [AFrom, AFields, AOutRowKey, AValues]);
end;

function TdxServerModeMSSQLAdapter.GetSelectSQLString(const AFields, AFrom,
  AWhere, AGroup, ASortInfo: string; ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string;
var
  AModificatorsSql: string;
begin
  if (ATopRecords > 0) and (ASkipRecords = 0) then
    AModificatorsSql := Format('TOP %d ', [ATopRecords])
  else
    AModificatorsSql := '';
  if ADistinct then
    AModificatorsSql := 'DISTINCT ' + AModificatorsSql;
  if ASkipRecords = 0 then
    Result := Format('SELECT %s%s FROM %s %s %s %s', [AModificatorsSql, AFields, AFrom, AWhere, AGroup, ASortInfo])
  else
  begin
    Result := Format('SELECT %s FROM (SELECT %0:s, ROW_NUMBER() OVER(%3:s) as "DXROWNUMBER" FROM %1:s %2:s)DXRESULTSET ' +
      'WHERE DXRESULTSET.DXROWNUMBER > %4:d', [AFields, AFrom, AWhere, ASortInfo, ASkipRecords]);
    if ATopRecords > 0 then
      Result := Format('%s AND DXRESULTSET.DXROWNUMBER <= %d', [Result, ASkipRecords + ATopRecords]);
  end;
end;

function TdxServerModeMSSQLAdapter.GetUpdateSQLString(const AFrom, AOutRowKey, AValues, AWhere: string): string;
begin
  Result := Format('UPDATE %s SET %s OUTPUT %s %s', [AFrom, AValues, AOutRowKey, AWhere]);
end;

function TdxServerModeMSSQLAdapter.MakeEscapedValue(const AValue: string; AEscapePercentWildcard,
  AEscapeUnderscoreWildcard: Boolean): string;
begin
  Result := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '[', '\[', [rfReplaceAll]);
  Result := StringReplace(Result, ']', '\]', [rfReplaceAll]);
  if AEscapePercentWildcard then
    Result := StringReplace(Result, GetPercentWildcard, '\' + GetPercentWildcard, [rfReplaceAll]);
  if AEscapeUnderscoreWildcard then
    Result := StringReplace(Result, GetUnderscoreWildcard, '\' + GetUnderscoreWildcard, [rfReplaceAll]);
end;

{ TdxServerModeMySQLAdapter }

class function TdxServerModeMySQLAdapter.GetDisplayName: string;
begin
  Result := 'MySQL Adapter';
end;

function TdxServerModeMySQLAdapter.GetSQLQueryKeyFieldNames(const ATableName: string): string;
begin
  Result := Format('SHOW COLUMNS FROM `%s` WHERE `Key`=''PRI''', [ATableName]);
end;

procedure TdxServerModeMySQLAdapter.CheckParams(AParams: TdxServerModeParams; var ACommandText: string);
begin
  CheckRepeatedParams(AParams, ACommandText);
  SortParams(AParams, ACommandText);
  RenameParams(AParams, ACommandText);
end;

procedure TdxServerModeMySQLAdapter.CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  APos: Integer;
  ANewName: string;
  AParamName: string;
  AParam: TParam;
  ADisplayValue: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    APos := PosEx(AParamName, ACommandText, 1);
    while APos > 0 do
    begin
      APos := PosEx(AParamName, ACommandText, APos + 1);
      if APos > 0 then
      begin
        AParam := CreateParam(AParams, AParams[I].Value, AParams[I].DataType, ADisplayValue);
        AParam.Name := Format(GetParamFakeNameFormat, [AParam.Index + 1]);
        ANewName := Format(GetParamFormat, [AParam.Name]);
        ACommandText := Copy(ACommandText, 1, APos - 1) + ANewName +
          Copy(ACommandText, APos + Length(AParamName), Length(ACommandText) - APos - Length(AParamName) + 1);
      end;
    end;
  end;
end;

function TdxServerModeMySQLAdapter.GetEscapeTextForParamName: string;
begin
  Result := ' ESCAPE ' + QuotedString('\');
end;

function TdxServerModeMySQLAdapter.GetParamCheck: Boolean;
begin
  Result := False;
end;

procedure TdxServerModeMySQLAdapter.RenameParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  AParamName, ANewName: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    AParams[I].Name := Format(GetParamNameFormat, [AParams[I].Index + 1]);
    ANewName := '?';
    ACommandText := StringReplace(ACommandText, AParamName, ANewName, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TdxServerModeMySQLAdapter.GetCastAsDateTimeFormat: string;
begin
  Result := 'CAST(%s as DATETIME)';
end;

function TdxServerModeMySQLAdapter.GetCastAsTimeFormat: string;
begin
  Result := 'CAST(%s as TIME)';
end;

function TdxServerModeMySQLAdapter.GetSelectSQLString(const AFields, AFrom,
  AWhere, AGroup, ASortInfo: string; ATopRecords,
  ASkipRecords: Integer; ADistinct: Boolean): string;
var
  AModificatorsSql: string;
begin
  if ASkipRecords = 0 then
  begin
    if ATopRecords > 0 then
      AModificatorsSql := Format('LIMIT %d ', [ATopRecords])
    else
      AModificatorsSql := '';
  end
  else
  begin
    if ATopRecords = 0 then
      ATopRecords := MaxInt;
    AModificatorsSql := Format('LIMIT %d, %d', [ASkipRecords, ATopRecords]);
  end;
  if ADistinct then
    Result := Format('SELECT DISTINCT %s FROM %s %s %s %s %s', [AFields, AFrom, AWhere, AGroup, ASortInfo, AModificatorsSql])
  else
    Result := Format('SELECT %s FROM %s %s %s %s %s', [AFields, AFrom, AWhere, AGroup, ASortInfo, AModificatorsSql]);
end;

function TdxServerModeMySQLAdapter.GetQuoteChar: string;
begin
  Result := '`';
end;

function TdxServerModeMySQLAdapter.QuotedString(const AParamValue: string): string;
begin
  Result := inherited QuotedString(AParamValue);
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
end;

function TdxServerModeMySQLAdapter.DateDec(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  Result := Format('SUBDATE(%s, INTERVAL %s %s)', [ADateExpression, ADelta, GetDatePart(APart)]);
end;

function TdxServerModeMySQLAdapter.DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string;
begin
  case APart of
    dpYear:
      Result := Format('(Year(%s) - Year(%s))', [ADateExpression2, ADateExpression1]);
    dpMonth:
      Result := Format('(((Year(%s) - Year(%s)) * 12) + Month(%0:s) - Month(%1:s))', [ADateExpression2, ADateExpression1]);
    dpDay:
      Result := Format('DATEDIFF(%s, %s)', [ADateExpression2, ADateExpression1]);
    dpHour:
      Result := Format('(Hour(%s) - Hour(%s))', [ADateExpression2, ADateExpression1]);
  else
    Result := '(' + DatePart(APart, ADateExpression2) + ' - ' + DatePart(APart, ADateExpression1) + ')';
  end;
end;

function TdxServerModeMySQLAdapter.DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  Result := Format('ADDDATE(%s, INTERVAL %s %s)', [ADateExpression, ADelta, GetDatePart(APart)]);
end;

function TdxServerModeMySQLAdapter.DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string;
begin
  case APart of
    dpYear: Result := Format('Year(%s)', [ADateExpression]);
    dpMonth: Result := Format('Month(%s)', [ADateExpression]);
    dpDay: Result := Format('Day(%s)', [ADateExpression]);
    dpHour: Result := Format('Hour(%s)', [ADateExpression]);
    dpMinute: Result := Format('Minute(%s)', [ADateExpression]);
    dpSecond: Result := Format('Second(%s)', [ADateExpression]);
    dpMicrosecond: Result := Format('Microsecond(%s)', [ADateExpression]);
  else
    Result := Format('EXTRACT(%s FROM %s)', [GetDatePart(APart), ADateExpression]);
  end;
end;

function TdxServerModeMySQLAdapter.IsMicrosecondSupported: Boolean;
begin
  Result := True;
end;

function TdxServerModeMySQLAdapter.IsMillisecondSupported: Boolean;
begin
  Result := False;
end;

function TdxServerModeMySQLAdapter.NeedCastGroupingByDateRange: Boolean;
begin
  Result := True;
end;

function TdxServerModeMySQLAdapter.GetCaseFullFormat: string;
begin
  Result := 'if(%s, %s, %s)';
end;

{ TdxServerModeFirebirdAdapter }

class function TdxServerModeFirebirdAdapter.GetDisplayName: string;
begin
  Result := 'Firebird Adapter';
end;

function TdxServerModeFirebirdAdapter.GetSQLQueryKeyFieldNames(const ATableName: string): string;
begin
  Result := Format('SELECT RDB$FIELD_NAME,RDB$FIELD_POSITION FROM RDB$INDEX_SEGMENTS WHERE RDB$INDEX_NAME = ( ' +
    'SELECT RDB$INDEX_NAME FROM RDB$RELATION_CONSTRAINTS WHERE RDB$RELATION_NAME = ''%s'' AND RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'' ' +
    ') ORDER BY RDB$FIELD_POSITION;', [ATableName]);
end;

function TdxServerModeFirebirdAdapter.CanUseParam(AParam: TdxServerModeParam): Boolean;
begin
  Result := inherited CanUseParam(AParam) and not (AParam.CriteriaOperatorKind in
    [foLike, foNotLike, foContains, foNotContains, foBeginsWith, foEndsWith]);
end;

procedure TdxServerModeFirebirdAdapter.CheckParams(AParams: TdxServerModeParams; var ACommandText: string);
begin
  CheckRepeatedParams(AParams, ACommandText);
  SortParams(AParams, ACommandText);
  RenameParams(AParams, ACommandText);
end;

procedure TdxServerModeFirebirdAdapter.CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  APos: Integer;
  ANewName: string;
  AParamName: string;
  AParam: TParam;
  ADisplayValue: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    APos := PosEx(AParamName, ACommandText, 1);
    while APos > 0 do
    begin
      APos := PosEx(AParamName, ACommandText, APos + 1);
      if APos > 0 then
      begin
        AParam := CreateParam(AParams, AParams[I].Value, AParams[I].DataType, ADisplayValue);
        AParam.Name := Format(GetParamFakeNameFormat, [AParam.Index + 1]);
        ANewName := Format(GetParamFormat, [AParam.Name]);
        ACommandText := Copy(ACommandText, 1, APos - 1) + ANewName +
          Copy(ACommandText, APos + Length(AParamName), Length(ACommandText) - APos - Length(AParamName) + 1);
      end;
    end;
  end;
end;

function TdxServerModeFirebirdAdapter.GetParamCheck: Boolean;
begin
  Result := False;
end;

procedure TdxServerModeFirebirdAdapter.RenameParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  AParamName, ANewName: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    AParams[I].Name := Format(GetParamNameFormat, [AParams[I].Index + 1]);
    ANewName := '?';
    ACommandText := StringReplace(ACommandText, AParamName, ANewName, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TdxServerModeFirebirdAdapter.CanIdentifyInsertingRow: Boolean;
begin
  Result := True;
end;

function TdxServerModeFirebirdAdapter.CanUseResultFieldName: Boolean;
begin
  Result := True;
end;

function TdxServerModeFirebirdAdapter.DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string;
begin
  Result := Format('EXTRACT(%s FROM %s)', [GetDatePart(APart), ADateExpression]);
end;

function TdxServerModeFirebirdAdapter.GetCastAsDateTimeFormat: string;
begin
  Result := 'timestamp %s';
end;

function TdxServerModeFirebirdAdapter.GetCastAsTimeFormat: string;
begin
  Result := 'cast(cast(%s as timestamp) as time)';
end;

function TdxServerModeFirebirdAdapter.GetDateTimeFormat: string;
begin
  Result := 'yyyy-mm-dd hh:mm:ss.zzz';
end;

function TdxServerModeFirebirdAdapter.GetInsertSQLString(const AFrom, AOutRowKey, AFields, AValues: string): string;
begin
  Result := Format('INSERT INTO %s (%s) VALUES (%s) RETURNING %s', [AFrom, AFields, AValues, AOutRowKey]);
end;

function TdxServerModeFirebirdAdapter.GetUpdateSQLString(const AFrom, AOutRowKey, AValues, AWhere: string): string;
begin
  Result := Format('UPDATE %s SET %s %s RETURNING %s', [AFrom, AValues, AWhere, AOutRowKey]);
end;

function TdxServerModeFirebirdAdapter.GetFieldsRetrieveQueryFormat: string;
begin
  Result := inherited GetFieldsRetrieveQueryFormat + ';';
end;

function TdxServerModeFirebirdAdapter.GetSelectSQLString(const AFields, AFrom,
  AWhere, AGroup, ASortInfo: string; ATopRecords,
  ASkipRecords: Integer; ADistinct: Boolean): string;
var
  AModificatorsSql: string;
begin
  if ATopRecords > 0 then
    AModificatorsSql := Format('FIRST %d ', [ATopRecords])
  else
    AModificatorsSql := '';
  if ADistinct then
    AModificatorsSql := AModificatorsSql + 'DISTINCT ';
  if ASkipRecords > 0 then
    AModificatorsSql := Format('%sSKIP %d ', [AModificatorsSql, ASkipRecords]);
  Result := Format('SELECT %s%s FROM %s %s %s %s', [AModificatorsSql, AFields, AFrom, AWhere, AGroup, ASortInfo]);
end;

{ TdxServerModeInterBaseAdapter }

class function TdxServerModeInterBaseAdapter.GetDisplayName: string;
begin
  Result := 'InterBase Adapter';
end;

function TdxServerModeInterBaseAdapter.CanUseParam(AParam: TdxServerModeParam): Boolean;
begin
  Result := inherited CanUseParam(AParam) and not (AParam.CriteriaOperatorKind in
    [foLike, foNotLike, foContains, foNotContains, foBeginsWith, foEndsWith]);
end;

function TdxServerModeInterBaseAdapter.CanUseResultFieldName: Boolean;
begin
  Result := False;
end;

function TdxServerModeInterBaseAdapter.DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string;
begin
  Result := Format('EXTRACT(%s FROM %s)', [GetDatePart(APart), ADateExpression]);
end;

function TdxServerModeInterBaseAdapter.GetCastAsDateTimeFormat: string;
begin
  Result := 'CAST(%s as TIMESTAMP)';
end;

function TdxServerModeInterBaseAdapter.GetCastAsTimeFormat: string;
begin
  Result := 'CAST(CAST(%s as TIMESTAMP) as TIME)';
end;

function TdxServerModeInterBaseAdapter.IsDateRangeGroupingSupported: Boolean;
begin
  Result := False;
end;

function TdxServerModeInterBaseAdapter.GetSelectSQLString(const AFields, AFrom,
  AWhere, AGroup, ASortInfo: string; ATopRecords,
  ASkipRecords: Integer; ADistinct: Boolean): string;
var
  AModificatorsSql: string;
begin
  if ASkipRecords = 0 then
  begin
    if ATopRecords > 0 then
      AModificatorsSql := Format('ROWS %d ', [ATopRecords])
    else
      AModificatorsSql := '';
  end
  else
  begin
    if ATopRecords = 0 then
      ATopRecords := MaxInt
    else
      ATopRecords := ASkipRecords + ATopRecords;
    AModificatorsSql := Format('ROWS %d TO %d', [ASkipRecords + 1, ATopRecords]);
  end;
  if ADistinct then
    Result := Format('SELECT DISTINCT %s FROM %s %s %s %s %s', [AFields, AFrom, AWhere, AGroup, ASortInfo, AModificatorsSql])
  else
    Result := Format('SELECT %s FROM %s %s %s %s %s', [AFields, AFrom, AWhere, AGroup, ASortInfo, AModificatorsSql]);
end;

function TdxServerModeInterBaseAdapter.GetTableNameFormat: string;
begin
  Result := '%s';
end;

{ TdxServerModeOracleAdapter }

constructor TdxServerModeOracleAdapter.Create(
  ADataSource: TdxServerModeCustomDataSource);
begin
  inherited Create(ADataSource);
  FNullsSortOrder := nsoFirstIfDesc;
end;

procedure TdxServerModeOracleAdapter.Assign(Source: TPersistent);
begin
  if Source is TdxServerModeOracleAdapter then
    NullsSortOrder := TdxServerModeOracleAdapter(Source).NullsSortOrder
  else
    inherited Assign(Source);
end;

class function TdxServerModeOracleAdapter.GetDisplayName: string;
begin
  Result := 'Oracle Adapter';
end;

function TdxServerModeOracleAdapter.GetSQLQueryKeyFieldNames(const ATableName: string): string;
begin
  if GetSchemaName = '' then
    Result := Format('select tc.COLUMN_NAME from USER_CONS_COLUMNS tc ' +
              'left join USER_CONSTRAINTS c on tc.CONSTRAINT_NAME = c.CONSTRAINT_NAME ' +
              'where c.CONSTRAINT_TYPE = ''P'' and tc.TABLE_NAME = ''%s''', [ATableName])
  else
    Result := Format('select tc.COLUMN_NAME from ALL_CONS_COLUMNS tc ' +
                     'left join ALL_CONSTRAINTS c on tc.CONSTRAINT_NAME = c.CONSTRAINT_NAME ' +
                     'where c.CONSTRAINT_TYPE = ''P'' and c.OWNER = ''%s''  and tc.TABLE_NAME = ''%s''',
                      [GetSchemaName, ATableName]);
end;

function TdxServerModeOracleAdapter.GetSQLQuerySchemaNames: string;
begin
  Result := 'SELECT DISTINCT OWNER FROM ALL_OBJECTS ' +
            'ORDER BY 1';
end;

procedure TdxServerModeOracleAdapter.CheckParams(AParams: TdxServerModeParams; var ACommandText: string);
begin
  CheckRepeatedParams(AParams, ACommandText);
  SortParams(AParams, ACommandText);
  RenameParams(AParams, ACommandText);
end;

procedure TdxServerModeOracleAdapter.CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  APos: Integer;
  ANewName: string;
  AParamName: string;
  AParam: TParam;
  ADisplayValue: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    APos := PosEx(AParamName, ACommandText, 1);
    while APos > 0 do
    begin
      APos := PosEx(AParamName, ACommandText, APos + 1);
      if APos > 0 then
      begin
        AParam := CreateParam(AParams, AParams[I].Value, AParams[I].DataType, ADisplayValue);
        AParam.Name := Format(GetParamFakeNameFormat, [AParam.Index + 1]);
        ANewName := Format(GetParamFormat, [AParam.Name]);
        ACommandText := Copy(ACommandText, 1, APos - 1) + ANewName +
          Copy(ACommandText, APos + Length(AParamName), Length(ACommandText) - APos - Length(AParamName) + 1);
      end;
    end;
  end;
end;

function TdxServerModeOracleAdapter.GetOrderFormat(AIsDesc: Boolean): string;
begin
  Result := inherited GetOrderFormat(AIsDesc);
  if NullsSortOrder = nsoFirstIfAsc then
  begin
    if AIsDesc then
      Result := Result + ' NULLS LAST'
    else
      Result := Result + ' NULLS FIRST';
  end;
end;

function TdxServerModeOracleAdapter.GetParamCheck: Boolean;
begin
  Result := False;
end;

function TdxServerModeOracleAdapter.GetNullsSortOrder: TdxNullsSortOrder;
begin
  Result := NullsSortOrder;
end;

procedure TdxServerModeOracleAdapter.RenameParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  AParamName: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    AParams[I].Name := Format(GetParamNameFormat, [AParams[I].Index + 1]);
    ACommandText := StringReplace(ACommandText, AParamName, '?', [rfReplaceAll]);
  end;
end;

function TdxServerModeOracleAdapter.CanUseGroupingByDateRangeParams: Boolean;
begin
  Result := False;
end;

function TdxServerModeOracleAdapter.DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string;
begin
  case APart of
    dpMonth:
      Result := '((' + DatePart(dpYear, ADateExpression2) + ' - ' +
        DatePart(dpYear, ADateExpression1) + ') * 12 + ' +
        DatePart(dpMonth, ADateExpression2) + ' - ' +
        DatePart(dpMonth, ADateExpression1) + ')';
    dpDay:
      Result := Format('(TRUNC(%s) - TRUNC(%s))', [ADateExpression2, ADateExpression1]);
    dpHour:
      Result := Format('(TRUNC(%s, ''HH24'') - TRUNC(%s, ''HH24'')) * 24', [ADateExpression2, ADateExpression1]);
  else
    Result := '(' + DatePart(APart, ADateExpression2) + ' - ' + DatePart(APart, ADateExpression1) + ')';
  end;
end;

function TdxServerModeOracleAdapter.DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  case APart of
    dpYear:
      Result := Format('ADD_MONTHS(%s, (%s * 12))', [ADateExpression, ADelta]);
    dpMonth:
      Result := Format('ADD_MONTHS(%s, %s)', [ADateExpression, ADelta]);
    dpDay:
      Result := Format('%s + %s', [ADateExpression, ADelta]);
    dpHour:
      Result := Format('%s + (%s) / 24', [ADateExpression, ADelta]);
  else
    Result := '';
  end;
end;

function TdxServerModeOracleAdapter.DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string;
begin
  if APart in [dpHour, dpMinute, dpSecond] then
    Result := Format('EXTRACT(%s FROM CAST((%s) as TIMESTAMP))', [GetDatePart(APart), ADateExpression])
  else
    Result := Format('EXTRACT(%s FROM %s)', [GetDatePart(APart), ADateExpression]);
end;

function TdxServerModeOracleAdapter.GetCastAsDateTimeFormat: string;
begin
  Result := 'CAST(TO_TIMESTAMP(%s, ''yyyy-mm-dd hh24:mi:ss.ff'') as date)';
end;

function TdxServerModeOracleAdapter.GetCastAsTimeFormat: string;
begin
  Result := 'CAST(TO_TIMESTAMP(%s, ''yyyy-mm-dd hh24:mi:ss.ff'') as time)';
end;

function TdxServerModeOracleAdapter.GetDateTimeFormat: string;
begin
  Result := 'yyyy-mm-dd hh:mm:ss.zzz';
end;

function TdxServerModeOracleAdapter.TruncGroupingDate(const ADate: string; ADateTimeGrouping: TdxDateTimeGrouping): string;
begin
  case ADateTimeGrouping of
    dgByHour:
      Result := Format('TRUNC(%s, ''HH24'')', [ADate]);
    dgByDate:
      Result := Format('TRUNC(%s)', [ADate]);
    dgByWeekDay:
      Result := Format('TRUNC(%s, ''DY'')', [ADate]);
    dgByMonth:
      Result := Format('TRUNC(%s, ''MM'')', [ADate]);
    dgByYear:
      Result := Format('TRUNC(%s, ''YY'')', [ADate]);
  else
    Result := inherited TruncGroupingDate(ADate, ADateTimeGrouping);
  end;
end;

function TdxServerModeOracleAdapter.GetSchemaName: string;
begin
  Result := DataSource.Options.GetSchemaName;
end;


function TdxServerModeOracleAdapter.GetSelectSQLString(const AFields, AFrom,
  AWhere, AGroup, ASortInfo: string; ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string;
var
  AModificatorsSql: string;
begin
  if ADistinct then
    AModificatorsSql := 'DISTINCT '
  else
    AModificatorsSql := '';
  if ASkipRecords = 0 then
  begin
    if ATopRecords > 0 then
      Result := Format('SELECT * FROM (SELECT %s%s FROM %s %s %s %s) WHERE RowNum <= %d', [AModificatorsSql, AFields, AFrom, AWhere, AGroup, ASortInfo, ATopRecords])
    else
      Result := Format('SELECT %s%s FROM %s %s %s %s', [AModificatorsSql, AFields, AFrom, AWhere, AGroup, ASortInfo]);
    Exit;
  end
  else
  begin
     Result := Format('SELECT %1:s FROM(SELECT %1:s, RowNum rNum FROM(SELECT %0:s%1:s FROM %2:s %3:s %4:s %5:s)) WHERE rNum > %6:d', [AModificatorsSql, AFields, AFrom, AWhere, AGroup, ASortInfo, ASkipRecords]);
    if ATopRecords > 0 then
      Result := Format('%s and rNum <= %d', [Result, ATopRecords + ASkipRecords]);
  end;
end;


procedure TdxServerModeOracleAdapter.SetNullsSortOrder(
  const Value: TdxNullsSortOrder);
begin
  if FNullsSortOrder <> Value then
  begin
    FNullsSortOrder := Value;
    Changed([ctConnection]);
  end;
end;

{ TdxServerModePostgreSQLAdapter }

constructor TdxServerModePostgreSQLAdapter.Create(ADataSource: TdxServerModeCustomDataSource);
begin
  inherited Create(ADataSource);
  FNullsSortOrder := nsoFirstIfDesc;
end;

procedure TdxServerModePostgreSQLAdapter.Assign(Source: TPersistent);
begin
  if Source is TdxServerModePostgreSQLAdapter then
    NullsSortOrder := TdxServerModePostgreSQLAdapter(Source).NullsSortOrder
  else
    inherited Assign(Source);
end;

class function TdxServerModePostgreSQLAdapter.GetDisplayName: string;
begin
  Result := 'PostgreSQL Adapter';
end;

function TdxServerModePostgreSQLAdapter.GetSQLQueryKeyFieldNames(const ATableName: string): string;
begin
  if GetSchemaName = '' then
    Result := Format('select a.attname from pg_constraint c ' +
              'join pg_attribute a on c.conrelid=a.attrelid and a.attnum = ANY (c.conkey) ' +
              'join pg_class tc on c.conrelid=tc.oid ' +
              'join pg_namespace n on tc.relnamespace = n.oid ' +
              'where c.contype = ''p'' and tc.relname = ''%s'' ' +
              'order by a.attnum', [ATableName])
  else
    Result := Format('select a.attname from pg_constraint c ' +
                     'join pg_attribute a on c.conrelid=a.attrelid and a.attnum = ANY (c.conkey) ' +
                     'join pg_class tc on c.conrelid=tc.oid ' +
                     'join pg_namespace n on tc.relnamespace = n.oid ' +
                     'where c.contype = ''p'' and n.nspname = ''%s'' and tc.relname = ''%s''' +
                     'order by a.attnum', [GetSchemaName, ATableName]);
end;

function TdxServerModePostgreSQLAdapter.CanUseResultFieldName: Boolean;
begin
  Result := True;
end;

function TdxServerModePostgreSQLAdapter.CanUseGroupingByDateRangeParams: Boolean;
begin
  Result := False;
end;

procedure TdxServerModePostgreSQLAdapter.CheckParams(AParams: TdxServerModeParams; var ACommandText: string);
begin
  CheckRepeatedParams(AParams, ACommandText);
  SortParams(AParams, ACommandText);
  RenameParams(AParams, ACommandText);
end;

procedure TdxServerModePostgreSQLAdapter.CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  APos: Integer;
  ANewName: string;
  AParamName: string;
  AParam: TParam;
  ADisplayValue: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    APos := PosEx(AParamName, ACommandText, 1);
    while APos > 0 do
    begin
      APos := PosEx(AParamName, ACommandText, APos + 1);
      if APos > 0 then
      begin
        AParam := CreateParam(AParams, AParams[I].Value, AParams[I].DataType, ADisplayValue);
        AParam.Name := Format(GetParamFakeNameFormat, [AParam.Index + 1]);
        ANewName := Format(GetParamFormat, [AParam.Name]);
        ACommandText := Copy(ACommandText, 1, APos - 1) + ANewName +
          Copy(ACommandText, APos + Length(AParamName), Length(ACommandText) - APos - Length(AParamName) + 1);
      end;
    end;
  end;
end;

function TdxServerModePostgreSQLAdapter.GetOrderFormat(AIsDesc: Boolean): string;
begin
  Result := inherited GetOrderFormat(AIsDesc);
  if NullsSortOrder = nsoFirstIfAsc then
  begin
    if AIsDesc then
      Result := Result + ' NULLS LAST'
    else
      Result := Result + ' NULLS FIRST';
  end;
end;

function TdxServerModePostgreSQLAdapter.GetParamCheck: Boolean;
begin
  Result := False;
end;

function TdxServerModePostgreSQLAdapter.GetNullsSortOrder: TdxNullsSortOrder;
begin
  Result := NullsSortOrder;
end;

procedure TdxServerModePostgreSQLAdapter.RenameParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  AParamName, ANewName: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    AParams[I].Name := Format(GetParamNameFormat, [AParams[I].Index + 1]);
    ANewName := '?';
    ACommandText := StringReplace(ACommandText, AParamName, ANewName, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TdxServerModePostgreSQLAdapter.QuotedString(const AParamValue: string): string;
begin
  Result := StringReplace(AParamValue, '\', '\\', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '''', '\''', [rfReplaceAll]);
  Result := '''' + Result + '''';
end;

function TdxServerModePostgreSQLAdapter.CanIdentifyInsertingRow: Boolean;
begin
  Result := True;
end;

function TdxServerModePostgreSQLAdapter.DateDec(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  Result := Format('%s - %s', [ADateExpression, ADelta]);
end;

function TdxServerModePostgreSQLAdapter.DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string;
begin
  Result := Format('(date_trunc(''%s'', %s) - date_trunc(''%0:s'', %2:s))', [GetDatePart(APart), ADateExpression2, ADateExpression1]);
end;

function TdxServerModePostgreSQLAdapter.DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  Result := Format('%s + %s', [ADateExpression, ADelta]);
end;

function TdxServerModePostgreSQLAdapter.DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string;
begin
  Result := Format('date_part(''%s'', %s) * interval ''1 %0:s''', [GetDatePart(APart), ADateExpression]);
end;

function TdxServerModePostgreSQLAdapter.GetCastAsDateTimeFormat: string;
begin
  Result := 'TIMESTAMP %s';
end;

function TdxServerModePostgreSQLAdapter.GetCastAsTimeFormat: string;
begin
  Result := 'CAST(CAST(%s as TIMESTAMP) AS TIME)';

end;

function TdxServerModePostgreSQLAdapter.GetEscapeTextForParamName: string;
begin
  Result := ' ESCAPE ' + QuotedString('\');
end;

function TdxServerModePostgreSQLAdapter.GetInsertSQLString(const AFrom, AOutRowKey, AFields, AValues: string): string;
begin
  Result := Format('INSERT INTO %s (%s) VALUES (%s) RETURNING %s', [AFrom, AFields, AValues, AOutRowKey]);
end;

function TdxServerModePostgreSQLAdapter.GetUpdateSQLString(const AFrom, AOutRowKey, AValues, AWhere: string): string;
begin
  Result := Format('UPDATE %s SET %s %s RETURNING %s', [AFrom, AValues, AWhere, AOutRowKey]);
end;

function TdxServerModePostgreSQLAdapter.TruncGroupingDate(const ADate: string; ADateTimeGrouping: TdxDateTimeGrouping): string;
begin
  case ADateTimeGrouping of
    dgByHour:
      Result := Format('date_trunc(''hour'', %s)', [ADate]);
    dgByDate:
      Result := Format('date_trunc(''day'', %s)', [ADate]);
    dgByWeekDay:
      Result := Format('date_trunc(''week'', %s)', [ADate]);
    dgByMonth:
      Result := Format('date_trunc(''month'', %s)', [ADate]);
    dgByYear:
      Result := Format('date_trunc(''year'', %s)', [ADate]);
  else
    Result := inherited TruncGroupingDate(ADate, ADateTimeGrouping);
  end;
end;

procedure TdxServerModePostgreSQLAdapter.CheckFilterFieldCaption(var AFieldCaption: string;
  AOperatorKind: TcxFilterOperatorKind);
begin
  if AOperatorKind in [foLike, foContains, foNotContains, foBeginsWith, foEndsWith] then
    AFieldCaption := Format('CAST(%s as TEXT)', [AFieldCaption]);
end;

function TdxServerModePostgreSQLAdapter.GetSchemaName: string;
begin
  Result := DataSource.Options.GetSchemaName;
end;

function TdxServerModePostgreSQLAdapter.GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
  ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string;
var
  AModificatorsSql: string;
begin
  if ATopRecords > 0 then
    AModificatorsSql := Format('LIMIT %d ', [ATopRecords])
  else
    AModificatorsSql := '';
  if ASkipRecords > 0 then
    AModificatorsSql := Format('%s OFFSET %d ', [AModificatorsSql, ASkipRecords]);
  if ADistinct then
    Result := Format('SELECT DISTINCT %s FROM %s %s %s %s %s', [AFields, AFrom, AWhere, AGroup, ASortInfo, AModificatorsSql])
  else
    Result := Format('SELECT %s FROM %s %s %s %s %s', [AFields, AFrom, AWhere, AGroup, ASortInfo, AModificatorsSql]);
end;

procedure TdxServerModePostgreSQLAdapter.SetNullsSortOrder(const Value: TdxNullsSortOrder);
begin
  if FNullsSortOrder <> Value then
  begin
    FNullsSortOrder := Value;
    Changed([ctConnection]);
  end;
end;

{ TdxServerModeAdvantageSQLAdapter }

class function TdxServerModeAdvantageAdapter.GetDisplayName: string;
begin
  Result := 'Advantage Adapter';
end;

function TdxServerModeAdvantageAdapter.GetSQLQueryKeyFieldNames(const ATableName: string): string;
begin
  Result := Format('SELECT i.Index_Expression FROM system.indexes i INNER JOIN system.tables t ON t.Table_Primary_Key = i.Name WHERE t.Name = ''%s''',
    [ATableName]);
end;

function TdxServerModeAdvantageAdapter.IsMillisecondSupported: Boolean;
begin
  Result := False;
end;

function TdxServerModeAdvantageAdapter.DateDec(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  Result := Format('TIMESTAMPADD(%s, -(%s), CAST( %s as SQL_TIMESTAMP))', [GetDatePart(APart), ADelta, ADateExpression]);
end;

function TdxServerModeAdvantageAdapter.DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string;
begin
  if APart in [dpHour, dpMinute, dpSecond] then
    Result := Format('TIMESTAMPDIFF(%s, %s, %s)', [GetDatePart(APart), ADateExpression1, ADateExpression2])
  else
    Result := Format('TIMESTAMPDIFF(%s, CAST(%s as SQL_DATE), CAST(%s as SQL_DATE))', [GetDatePart(APart), ADateExpression1, ADateExpression2]);
end;

function TdxServerModeAdvantageAdapter.DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  Result := Format('TIMESTAMPADD(%s, %s, CAST( %s as SQL_TIMESTAMP))', [GetDatePart(APart), ADelta, ADateExpression]);
end;

function TdxServerModeAdvantageAdapter.DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string;
begin
  case APart of
    dpYear:
      Result := Format('EXTRACT(year FROM %s)', [ADateExpression]);
    dpQuarter:
      Result := Format('QUARTER(%s)', [ADateExpression]);
    dpMonth:
      Result := Format('EXTRACT(month FROM %s)', [ADateExpression]);
    dpDayOfYear:
      Result := Format('DAYOFYEAR(%s)', [ADateExpression]);
    dpDay:
      Result := Format('EXTRACT(day FROM %s)', [ADateExpression]);
    dpWeekDay:
      Result := Format('DAYOFWEEK(%s)', [ADateExpression]);
    dpWeek:
      Result := Format('WEEK(%s)', [ADateExpression]);
    dpHour:
      Result := Format('EXTRACT(hour FROM %s)', [ADateExpression]);
    dpMinute:
      Result := Format('EXTRACT(minute FROM %s)', [ADateExpression]);
    dpSecond:
      Result := Format('EXTRACT(second FROM %s)', [ADateExpression]);
  end;
end;

function TdxServerModeAdvantageAdapter.GetCastAsDateFormat: string;
begin
  Result := 'CAST( %s as SQL_DATE)';
end;

function TdxServerModeAdvantageAdapter.GetCastAsDateTimeFormat: string;
begin
  Result := 'CAST( %s as SQL_TIMESTAMP)';
end;

function TdxServerModeAdvantageAdapter.GetCastAsTimeFormat: string;
begin
  Result := 'CAST( %s as SQL_TIME)';
end;

function TdxServerModeAdvantageAdapter.GetDatePart(APart: TdxSQLDatePart): string;
begin
  case APart of
    dpYear:
      Result := 'SQL_TSI_YEAR';
    dpQuarter:
      Result := 'SQL_TSI_QUARTER';
    dpMonth:
      Result := 'SQL_TSI_MONTH';
    dpDayOfYear:
      Result := 'SQL_TSI_YEAR';
    dpDay:
      Result := 'SQL_TSI_DAY';
    dpWeek:
      Result := 'SQL_TSI_WEEK';
    dpHour:
      Result := 'SQL_TSI_HOUR';
    dpMinute:
      Result := 'SQL_TSI_MINUTE';
    dpSecond:
      Result := 'SQL_TSI_SECOND';
  end;
end;

function TdxServerModeAdvantageAdapter.GetEscapeTextForParamName: string;
begin
  Result := ' ESCAPE ' + QuotedString('\');
end;

function TdxServerModeAdvantageAdapter.GetDateTimeFormat: string;
begin
  Result := 'yyyy-mm-dd hh:mm:ss.zzz';
end;

procedure TdxServerModeAdvantageAdapter.CheckFilterFieldCaption(var AFieldCaption: string; AOperatorKind: TcxFilterOperatorKind);
begin
  if AOperatorKind in [foLike, foContains, foNotContains, foBeginsWith, foEndsWith] then
    AFieldCaption := Format('CAST(%s as SQL_CHAR)', [AFieldCaption]);
end;

function TdxServerModeAdvantageAdapter.CanUseResultFieldName: Boolean;
begin
  Result := True;
end;

function TdxServerModeAdvantageAdapter.CanUseGroupingByDateRangeParams: Boolean;
begin
  Result := False;
end;

procedure TdxServerModeAdvantageAdapter.CheckParams(AParams: TdxServerModeParams; var ACommandText: string);
begin
  CheckRepeatedParams(AParams, ACommandText);
  SortParams(AParams, ACommandText);
  RenameParams(AParams, ACommandText);
end;

procedure TdxServerModeAdvantageAdapter.CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  APos: Integer;
  ANewName: string;
  AParamName: string;
  AParam: TParam;
  ADisplayValue: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    APos := PosEx(AParamName, ACommandText, 1);
    while APos > 0 do
    begin
      APos := PosEx(AParamName, ACommandText, APos + 1);
      if APos > 0 then
      begin
        AParam := CreateParam(AParams, AParams[I].Value, AParams[I].DataType, ADisplayValue);
        AParam.Name := Format(GetParamFakeNameFormat, [AParam.Index + 1]);
        ANewName := Format(GetParamFormat, [AParam.Name]);
        ACommandText := Copy(ACommandText, 1, APos - 1) + ANewName +
          Copy(ACommandText, APos + Length(AParamName), Length(ACommandText) - APos - Length(AParamName) + 1);
      end;
    end;
  end;
end;

function TdxServerModeAdvantageAdapter.GetParamCheck: Boolean;
begin
  Result := False;
end;

procedure TdxServerModeAdvantageAdapter.RenameParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  AParamName, ANewName: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    AParams[I].Name := Format(GetParamNameFormat, [AParams[I].Index + 1]);
    ANewName := '?';
    ACommandText := StringReplace(ACommandText, AParamName, ANewName, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TdxServerModeAdvantageAdapter.GetFieldsRetrieveQueryFormat: string;
begin
  Result := 'SELECT * FROM %s WHERE NOT 1 = 1';
end;

function TdxServerModeAdvantageAdapter.GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
  ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string;
var
  AModificatorsSql: string;
begin
  if ATopRecords > 0 then
    AModificatorsSql := Format('TOP %d ', [ATopRecords])
  else
    AModificatorsSql := '';
  if ASkipRecords > 0 then
    AModificatorsSql := Format('%s START AT %d ', [AModificatorsSql, ASkipRecords + 1]);
  if ADistinct then
    AModificatorsSql := AModificatorsSql + 'DISTINCT ';
  Result := Format('SELECT %s %s FROM %s %s %s %s', [AModificatorsSql, AFields, AFrom, AWhere, AGroup, ASortInfo]);
end;

{ TdxServerModeMSAccessAdapter }

class function TdxServerModeMSAccessAdapter.GetDisplayName: string;
begin
  Result := 'MSAccess Adapter';
end;

procedure TdxServerModeMSAccessAdapter.CheckParams(AParams: TdxServerModeParams; var ACommandText: string);
begin
  CheckRepeatedParams(AParams, ACommandText);
  SortParams(AParams, ACommandText);
  RenameParams(AParams, ACommandText);
end;

procedure TdxServerModeMSAccessAdapter.CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  APos: Integer;
  ANewName: string;
  AParamName: string;
  AParam: TParam;
  ADisplayValue: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    APos := PosEx(AParamName, ACommandText, 1);
    while APos > 0 do
    begin
      APos := PosEx(AParamName, ACommandText, APos + 1);
      if APos > 0 then
      begin
        AParam := CreateParam(AParams, AParams[I].Value, AParams[I].DataType, ADisplayValue);
        AParam.Name := Format(GetParamFakeNameFormat, [AParam.Index + 1]);
        ANewName := Format(GetParamFormat, [AParam.Name]);
        ACommandText := Copy(ACommandText, 1, APos - 1) + ANewName +
          Copy(ACommandText, APos + Length(AParamName), Length(ACommandText) - APos - Length(AParamName) + 1);
      end;
    end;
  end;
end;

function TdxServerModeMSAccessAdapter.DateDec(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  Result := Format('DateAdd(''%s'', -(%s), %s)', [GetDatePart(APart), ADelta, ADateExpression]);
end;

function TdxServerModeMSAccessAdapter.DateDiff(APart: TdxSQLDatePart; const ADateExpression1, ADateExpression2: string): string;
begin
  Result := Format('DateDiff(''%s'', %s, %s)', [GetDatePart(APart), ADateExpression1, ADateExpression2]);
end;

function TdxServerModeMSAccessAdapter.DateInc(APart: TdxSQLDatePart; const ADateExpression, ADelta: string): string;
begin
  Result := Format('DateAdd(''%s'', %s, %s)', [GetDatePart(APart), ADelta, ADateExpression]);
end;

function TdxServerModeMSAccessAdapter.DatePart(APart: TdxSQLDatePart; const ADateExpression: string): string;
begin
  Result := Format('DatePart(''%s'', %s)', [GetDatePart(APart), ADateExpression]);
end;

function TdxServerModeMSAccessAdapter.DateTimeToString(const ADateTime: TDateTime): string;
begin
  SysUtils.DateTimeToString(Result, GetDateTimeFormat, ADateTime);
end;

function TdxServerModeMSAccessAdapter.GetCastAsDateTimeFormat: string;
begin
  Result := 'CDate(%s)';
end;

function TdxServerModeMSAccessAdapter.GetCastAsTimeFormat: string;
begin
  Result := '%s';
end;

function TdxServerModeMSAccessAdapter.GetCaseFullFormat: string;
begin
  Result := 'IIF(%s, %s, %s)';
end;

function TdxServerModeMSAccessAdapter.GetDatePart(APart: TdxSQLDatePart): string;
begin
  case APart of
    dpYear:
      Result := 'yyyy';
    dpQuarter:
      Result := 'q';
    dpMonth:
      Result := 'm';
    dpDayOfYear:
      Result := 'y';
    dpDay:
      Result := 'd';
    dpWeekDay:
      Result := 'w';
    dpWeek:
      Result := 'ww';
    dpHour:
      Result := 'h';
    dpMinute:
      Result := 'n';
    dpSecond:
      Result := 's';
  end;
end;

function TdxServerModeMSAccessAdapter.IsNull(const AExpression: string): string;
begin
  Result := Format('ISNULL(%s)', [AExpression]);
end;

function TdxServerModeMSAccessAdapter.MakeEscapedValue(const AValue: string; AEscapePercentWildcard,
  AEscapeUnderscoreWildcard: Boolean): string;

  function DoMakeEscapedValue(const S: string): string;
  var
    ASearchStr, ANewStr: string;
    I, AOffset: Integer;
  begin
    ASearchStr := S;
    ANewStr := S;
    Result := '';
    while ASearchStr <> '' do
    begin
      AOffset := AnsiPos('[', ASearchStr);
      I := AnsiPos(']', ASearchStr);
      if (AOffset = 0) or (I > 0) and (AOffset > I) then
        AOffset := I;
      if AOffset = 0 then
      begin
        Result := Result + ANewStr;
        Break;
      end;
      if ASearchStr[AOffset] = '[' then
        Result := Result + Copy(ANewStr, 1, AOffset - 1) + '[[]'
      else
        Result := Result + Copy(ANewStr, 1, AOffset - 1) + '[]]';
      ANewStr := Copy(ANewStr, AOffset + 1, MaxInt);
      ASearchStr := Copy(ASearchStr, AOffset + 1, MaxInt);
    end;
  end;

begin
  Result := DoMakeEscapedValue(AValue);
  if AEscapePercentWildcard then
    Result := StringReplace(Result, GetPercentWildcard, '[' + GetPercentWildcard + ']', [rfReplaceAll]);
  if AEscapeUnderscoreWildcard then
    Result := StringReplace(Result, GetUnderscoreWildcard, '[' + GetUnderscoreWildcard + ']', [rfReplaceAll]);
end;

function TdxServerModeMSAccessAdapter.NeedEscapeFakeParamName(AParam: TdxServerModeParam): Boolean;
begin
  Result := False;
end;

function TdxServerModeMSAccessAdapter.CanUseParam(AParam: TdxServerModeParam): Boolean;
begin
  Result := inherited CanUseParam(AParam) and not VarIsDate(AParam.Value);
end;

function TdxServerModeMSAccessAdapter.CanUseGroupingByDateRangeParams: Boolean;
begin
  Result := False;
end;

function TdxServerModeMSAccessAdapter.GetDateTimeFormat: string;
begin
  Result := '#mm''/''dd''/''yyyy hh'':''mm'':''ss#';
end;

function TdxServerModeMSAccessAdapter.GetFieldNameFormat: string;
begin
  Result := '[%s]';
end;

function TdxServerModeMSAccessAdapter.GetParamCheck: Boolean;
begin
  Result := False;
end;

function TdxServerModeMSAccessAdapter.GetTableNameFormat: string;
begin
  Result := '[%s]';
end;

function TdxServerModeMSAccessAdapter.IsMillisecondSupported: Boolean;
begin
  Result := False;
end;

function TdxServerModeMSAccessAdapter.IsSkipClauseSupported: Boolean;
begin
  Result := False;
end;

procedure TdxServerModeMSAccessAdapter.RenameParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  AParamName, ANewName: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    AParams[I].Name := Format(GetParamNameFormat, [AParams[I].Index + 1]);
    ANewName := '?';
    ACommandText := StringReplace(ACommandText, AParamName, ANewName, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TdxServerModeMSAccessAdapter.GetQuoteChar: string;
begin
  Result := '';
end;

function TdxServerModeMSAccessAdapter.GetSelectSQLString(const AFields, AFrom, AWhere, AGroup, ASortInfo: string;
  ATopRecords, ASkipRecords: Integer; ADistinct: Boolean): string;
var
  AModificatorsSql: string;
begin
  if ATopRecords > 0 then
    AModificatorsSql := Format('TOP %d ', [ATopRecords])
  else
    AModificatorsSql := '';
  if ADistinct then
    AModificatorsSql := 'DISTINCT ' + AModificatorsSql;
  if ASkipRecords > 0 then
    AModificatorsSql := Format('TOP %d ', [ATopRecords + ASkipRecords]);
  Result := Format('SELECT %s %s FROM %s %s %s %s', [AModificatorsSql, AFields, AFrom, AWhere, AGroup, ASortInfo]);
end;

{ TdxServerModeSQLiteAdapter }

class function TdxServerModeSQLiteAdapter.GetDisplayName: string;
begin
  Result := 'SQLite Adapter';
end;

procedure TdxServerModeSQLiteAdapter.CheckParams(AParams: TdxServerModeParams; var ACommandText: string);
begin
  CheckRepeatedParams(AParams, ACommandText);
  SortParams(AParams, ACommandText);
  RenameParams(AParams, ACommandText);
end;

procedure TdxServerModeSQLiteAdapter.CheckRepeatedParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  APos: Integer;
  ANewName: string;
  AParamName: string;
  AParam: TParam;
  ADisplayValue: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    APos := PosEx(AParamName, ACommandText, 1);
    while APos > 0 do
    begin
      APos := PosEx(AParamName, ACommandText, APos + 1);
      if APos > 0 then
      begin
        AParam := CreateParam(AParams, AParams[I].Value, AParams[I].DataType, ADisplayValue);
        AParam.Name := Format(GetParamFakeNameFormat, [AParam.Index + 1]);
        ANewName := Format(GetParamFormat, [AParam.Name]);
        ACommandText := Copy(ACommandText, 1, APos - 1) + ANewName +
          Copy(ACommandText, APos + Length(AParamName), Length(ACommandText) - APos - Length(AParamName) + 1);
      end;
    end;
  end;
end;

function TdxServerModeSQLiteAdapter.GetParamCheck: Boolean;
begin
  Result := False;
end;

procedure TdxServerModeSQLiteAdapter.RenameParams(AParams: TdxServerModeParams; var ACommandText: string);
var
  I: Integer;
  AParamName, ANewName: string;
begin
  for I := 0 to AParams.Count - 1 do
  begin
    AParamName := Format(GetParamFormat, [AParams[I].Name]);
    AParams[I].Name := Format(GetParamNameFormat, [AParams[I].Index + 1]);
    ANewName := '?';
    ACommandText := StringReplace(ACommandText, AParamName, ANewName, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TdxServerModeSQLiteAdapter.CanUseResultFieldName: Boolean;
begin
  Result := True;
end;

function TdxServerModeSQLiteAdapter.GetDateTimeFormat: string;
begin
  Result := 'yyyy-mm-dd hh:mm:ss';
end;

function TdxServerModeSQLiteAdapter.IsDateRangeGroupingSupported: Boolean;
begin
  Result := False;
end;

function TdxServerModeSQLiteAdapter.GetSelectSQLString(const AFields, AFrom,
  AWhere, AGroup, ASortInfo: string; ATopRecords,
  ASkipRecords: Integer; ADistinct: Boolean): string;
var
  AModificatorsSql: string;
begin
  if ATopRecords > 0 then
    AModificatorsSql := Format('LIMIT %d ', [ATopRecords])
  else
    AModificatorsSql := '';
  if ASkipRecords > 0 then
    AModificatorsSql := Format('%s OFFSET %d ', [AModificatorsSql, ASkipRecords]);
  if ADistinct then
    Result := Format('SELECT DISTINCT %s FROM %s %s %s %s %s', [AFields, AFrom, AWhere, AGroup, ASortInfo, AModificatorsSql])
  else
    Result := Format('SELECT %s FROM %s %s %s %s %s', [AFields, AFrom, AWhere, AGroup, ASortInfo, AModificatorsSql]);
end;

procedure RegisterAssistants;
begin
  TdxServerModeMSSQLAdapter.Register;
  TdxServerModeMySQLAdapter.Register;
  TdxServerModeFirebirdAdapter.Register;
  TdxServerModeInterBaseAdapter.Register;
  TdxServerModeOracleAdapter.Register;
  TdxServerModePostgreSQLAdapter.Register;
  TdxServerModeAdvantageAdapter.Register;
  TdxServerModeMSAccessAdapter.Register;
  TdxServerModeSQLiteAdapter.Register;
end;

procedure UnregisterAssistants;
begin
  TdxServerModeSQLiteAdapter.Unregister;
  TdxServerModeMSAccessAdapter.Unregister;
  TdxServerModeAdvantageAdapter.Unregister;
  TdxServerModePostgreSQLAdapter.Unregister;
  TdxServerModeOracleAdapter.Unregister;
  TdxServerModeInterBaseAdapter.Unregister;
  TdxServerModeFirebirdAdapter.Unregister;
  TdxServerModeMySQLAdapter.Unregister;
  TdxServerModeMSSQLAdapter.Unregister;
end;

initialization
  RegisterAssistants;

finalization
  UnregisterAssistants;

end.

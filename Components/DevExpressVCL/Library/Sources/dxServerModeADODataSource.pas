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

unit dxServerModeADODataSource;

{$I cxVer.inc}

interface

uses
  Variants, SysUtils, Classes, ADODB, DB, cxFilter, dxServerModeClasses,
  dxServerModeData;

type
  TdxServerModeADODataSource = class;

  { TdxServerModeADOSQLQueryBuilder }

  TdxServerModeADOSQLQueryBuilder = class(TdxServerModeCustomSQLQueryBuilder)
  protected
    function CanUseParam(AParam: TdxServerModeParam): Boolean; override;
  end;

  { TdxServerModeADODataSourceOptions }

  TdxServerModeADODataSourceOptions = class(TdxServerModeDataSourceOptions)
  private
    function GetEnableBCD: Boolean;
    procedure SetEnableBCD(const Value: Boolean);
    function GetDataSource: TdxServerModeADODataSource;
  protected
    property DataSource: TdxServerModeADODataSource read GetDataSource;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property EnableBCD: Boolean read GetEnableBCD write SetEnableBCD default True;
  end;

  { TdxServerModeADODataSourceHelper }

  TdxServerModeADODataSourceHelper = class(TdxServerModeDataSourceCustomHelper)
  protected
    function GetDataSetClass: TdxDataSetClass; override;
    function GetOptionsClass: TdxServerModeDataSourceOptionsClass; override;
    function GetQueryBuilderClass: TdxServerModeCustomSQLQueryBuilderClass; override;

    procedure DoExecuteCommand(AResultSet: PDataSet; const ACommand: string; AParams: TParams; AParamCheck: Boolean); override;
    procedure DoInitializeDataSet(ADataSet: TDataSet); override;
    procedure DoSynchronizeDataSetConnection(ADataSet: TDataSet); override;
    procedure PrepareDatasetForIterate(ADataSet: TDataSet); override;
  end;

  { TdxServerModeADODataSource }

  TdxServerModeADODataSource = class(TdxServerModeDataSource)
  private
    function GetConnection: TADOConnection;
    function GetDataSet: TADODataSet;
    function GetOptions: TdxServerModeADODataSourceOptions;
    procedure SetConnection(Value: TADOConnection);
    procedure SetOptions(const Value: TdxServerModeADODataSourceOptions);
  protected
    procedure DoPopulateKeyFields(AList: TStrings); override;
    procedure DoPopulateTableNames(AList: TStrings); override;
    procedure DoPopulateViewNames(AList: TStrings); override;
    function GetHelperClass: TdxServerModeDataSourceCustomHelperClass; override;

    property DataSet: TADODataSet read GetDataSet;
  published
    property Connection: TADOConnection read GetConnection write SetConnection;
    property Options: TdxServerModeADODataSourceOptions read GetOptions write SetOptions;
  end;

  { TdxServerModeADOQueryDataSource }

  TdxServerModeADOQueryDataSource = class(TdxServerModeQueryDataSource)
  private
    function GetConnection: TADOConnection;
    function GetDataSet: TADODataSet;
    function GetOptions: TdxServerModeADODataSourceOptions;
    procedure SetConnection(Value: TADOConnection);
    procedure SetOptions(const Value: TdxServerModeADODataSourceOptions);
  protected
    function GetHelperClass: TdxServerModeDataSourceCustomHelperClass; override;

    property DataSet: TADODataSet read GetDataSet;
  published
    property Connection: TADOConnection read GetConnection write SetConnection;
    property Options: TdxServerModeADODataSourceOptions read GetOptions write SetOptions;
  end;

implementation

uses
  ADOInt, cxVariants, Math;

type
  TADOConnectionAccess = class(TADOConnection);

function FieldTypeToADOType(const AFieldType: TFieldType): DataTypeEnum;
begin
  case AFieldType of
    ftString: Result := adVarChar;
    ftWideString: Result := adVarWChar;
    ftSmallint: Result := adSmallint;
    ftInteger, ftAutoInc: Result := adInteger;
    ftWord: Result := adUnsignedSmallInt;
    ftBoolean: Result := adBoolean;
    ftFloat: Result := adDouble;
    ftCurrency, ftBCD, ftFMTBCD: Result := adCurrency;
    ftDate: Result := adDBDate;
    ftTime: Result := adDBTime;
    ftDateTime: Result := adDBTimeStamp;
    ftBytes: Result := adBinary;
    ftVarBytes: Result := adVarBinary;
    ftMemo: Result := adLongVarChar;
    ftBlob, ftGraphic..ftTypedBinary: Result := adLongVarBinary;
    ftFixedChar: Result := adChar;
    ftLargeint: Result := adBigInt;
    ftVariant: Result := adVariant;
    ftInterface: Result := adIUnknown;
    ftIDispatch: Result := adIDispatch;
    ftGuid: Result := adGUID;
    ftFixedWideChar: Result := adWChar;
    ftWideMemo: Result := adLongVarWChar;
  else
    Result := adEmpty;
  end;
end;

{ TdxServerModeADOSQLQueryBuilder }

function TdxServerModeADOSQLQueryBuilder.CanUseParam(AParam: TdxServerModeParam): Boolean;
var
  AHour, AMinute, ASecond, AMillisecond: Word;
begin
  Result := inherited CanUseParam(AParam) and
    (not VarIsStr(AParam.Value) or (Length(AParam.Value) > 0));
  if Result and VarIsDate(AParam.Value) then
  begin
    DecodeTime(AParam.Value, AHour, AMinute, ASecond, AMillisecond);
    Result := AMillisecond = 0;
  end;
end;

{ TdxServerModeADODataSourceOptions }

procedure TdxServerModeADODataSourceOptions.Assign(Source: TPersistent);
begin
  if Source is TdxServerModeADODataSourceOptions then
    EnableBCD := TdxServerModeADODataSourceOptions(Source).EnableBCD;
  inherited Assign(Source);
end;

function TdxServerModeADODataSourceOptions.GetDataSource: TdxServerModeADODataSource;
begin
  Result := TdxServerModeADODataSource(Owner);
end;

function TdxServerModeADODataSourceOptions.GetEnableBCD: Boolean;
begin
  Result := DataSource.DataSet.EnableBCD;
end;

procedure TdxServerModeADODataSourceOptions.SetEnableBCD(const Value: Boolean);
begin
  DataSource.DataSet.EnableBCD := Value;
end;

{ TdxServerModeADODataSourceHelper }

function TdxServerModeADODataSourceHelper.GetDataSetClass: TdxDataSetClass;
begin
  Result := TADODataSet;
end;

function TdxServerModeADODataSourceHelper.GetOptionsClass: TdxServerModeDataSourceOptionsClass;
begin
  Result := TdxServerModeADODataSourceOptions;
end;

function TdxServerModeADODataSourceHelper.GetQueryBuilderClass: TdxServerModeCustomSQLQueryBuilderClass;
begin
  Result := TdxServerModeADOSQLQueryBuilder;
end;

procedure TdxServerModeADODataSourceHelper.DoExecuteCommand(AResultSet: PDataSet; const ACommand: string; AParams: TParams; AParamCheck: Boolean);
var
  ADOCommand: TADOCommand;
  I: Integer;
  AParameter: TParameter;
  ARecordSet: _RecordSet;
begin
  ADOCommand := TADOCommand.Create(nil);
  try
    ADOCommand.Connection := TADOConnection(Connection);
    ADOCommand.ParamCheck := AParamCheck;
    ADOCommand.CommandText := ACommand;
    if (AParams <> nil) and (AParams.Count > 0) then
      for I := 0 to AParams.Count - 1 do
      begin
        AParameter := ADOCommand.Parameters.FindParam(AParams[I].Name);
        if AParameter = nil then
          AParameter := ADOCommand.Parameters.AddParameter;
        AParameter.ParameterObject.Type_ := FieldTypeToADOType(AParams[I].DataType);
        AParameter.Name := AParams[I].Name;
        AParameter.Value := AParams[I].Value;
      end;
    if AResultSet <> nil then
    begin
      ARecordSet := ADOCommand.Execute;
      if (ARecordSet.State and adStateOpen) = adStateOpen then
      begin
        AResultSet^ := GetDataSetClass.Create(nil);
        TADODataSet(AResultSet^).RecordSet := ARecordSet;
      end;
    end
    else
    begin
      ADOCommand.ExecuteOptions := ADOCommand.ExecuteOptions + [eoExecuteNoRecords];
      ADOCommand.Execute;
    end;
  finally
    ADOCommand.Free;
  end;
end;

procedure TdxServerModeADODataSourceHelper.DoInitializeDataSet(ADataSet: TDataSet);
begin
  TADODataSet(ADataSet).CommandText := GetFieldsRetrieveQuery;
end;

procedure TdxServerModeADODataSourceHelper.DoSynchronizeDataSetConnection(ADataSet: TDataSet);
begin
  TADODataSet(ADataSet).Connection := TADOConnection(Connection);
end;

procedure TdxServerModeADODataSourceHelper.PrepareDatasetForIterate(ADataSet: TDataSet);
begin
  inherited PrepareDatasetForIterate(ADataSet);
  TADODataSet(ADataSet).CacheSize := Max(ADataSet.RecordCount, 1);
end;

{ TdxServerModeADODataSource }

procedure TdxServerModeADODataSource.DoPopulateKeyFields(AList: TStrings);
const
  COLUMN_NAME = 'COLUMN_NAME';
var
  AFields: _Recordset;
begin
  TADOConnectionAccess(Connection).CheckActive;
  AFields := Connection.ConnectionObject.OpenSchema(adSchemaPrimaryKeys, VarArrayOf([Null, Null, TableName]),
    EmptyParam);
  while not AFields.EOF do
  begin
    AList.Add(VarToStr(AFields.Fields[COLUMN_NAME].Value));
    AFields.MoveNext;
  end;
end;

procedure TdxServerModeADODataSource.DoPopulateTableNames(AList: TStrings);
begin
  Connection.GetTableNames(AList);
end;

procedure TdxServerModeADODataSource.DoPopulateViewNames(AList: TStrings);
var
  ATypeField, ANameField: TField;
  ATableType: string;
  ADataSet: TADODataSet;
begin
  ADataSet := TADODataSet.Create(nil);
  try
    Connection.OpenSchema(siTables, EmptyParam, EmptyParam, ADataSet);
    ATypeField := ADataSet.FieldByName('TABLE_TYPE');
    ANameField := ADataSet.FieldByName('TABLE_NAME');
    AList.BeginUpdate;
    try
      while not ADataSet.EOF do
      begin
        ATableType := ATypeField.AsString;
        if ATableType = 'VIEW' then
          AList.Add(ANameField.AsWideString);
        ADataSet.Next;
      end;
    finally
      AList.EndUpdate;
    end;
  finally
    ADataSet.Free;
  end;
end;

function TdxServerModeADODataSource.GetHelperClass: TdxServerModeDataSourceCustomHelperClass;
begin
  Result := TdxServerModeADODataSourceHelper;
end;

function TdxServerModeADODataSource.GetOptions: TdxServerModeADODataSourceOptions;
begin
  Result := TdxServerModeADODataSourceOptions(inherited Options);
end;

function TdxServerModeADODataSource.GetConnection: TADOConnection;
begin
  Result := TADOConnection(inherited Connection);
end;

function TdxServerModeADODataSource.GetDataSet: TADODataSet;
begin
  Result := TADODataSet(inherited DataSet);
end;

procedure TdxServerModeADODataSource.SetConnection(Value: TADOConnection);
begin
  inherited Connection := Value;
end;

procedure TdxServerModeADODataSource.SetOptions(const Value: TdxServerModeADODataSourceOptions);
begin
  inherited Options := Value;
end;

{ TdxServerModeADOQueryDataSource }

function TdxServerModeADOQueryDataSource.GetHelperClass: TdxServerModeDataSourceCustomHelperClass;
begin
  Result := TdxServerModeADODataSourceHelper;
end;

function TdxServerModeADOQueryDataSource.GetOptions: TdxServerModeADODataSourceOptions;
begin
  Result := TdxServerModeADODataSourceOptions(inherited Options);
end;

function TdxServerModeADOQueryDataSource.GetConnection: TADOConnection;
begin
  Result := TADOConnection(inherited Connection);
end;

function TdxServerModeADOQueryDataSource.GetDataSet: TADODataSet;
begin
  Result := TADODataSet(inherited DataSet);
end;

procedure TdxServerModeADOQueryDataSource.SetConnection(Value: TADOConnection);
begin
  inherited Connection := Value;
end;

procedure TdxServerModeADOQueryDataSource.SetOptions(const Value: TdxServerModeADODataSourceOptions);
begin
  inherited Options := Value;
end;

end.

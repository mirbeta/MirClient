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

unit dxServerModeDBXDataSource;

{$I cxVer.inc}

interface

uses
  Variants, SysUtils, Classes, SqlExpr, DB,
  dxServerModeClasses, dxServerModeData, cxFilter;

type

  TdxServerModeDBXDataSource = class;

  { TdxServerModeDBXSQLQueryBuilder }

  TdxServerModeDBXSQLQueryBuilder = class(TdxServerModeCustomSQLQueryBuilder)
  protected
    function CanUseParams: Boolean; override;
  end;

  { TdxServerModeDBXDataSourceOptions }

  TdxServerModeDBXDataSourceOptions = class(TdxServerModeDataSourceOptions)
  private
    FSchemaName: string;
    procedure SetSchemaName(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    function GetSchemaName: string; override;
  published
    property SchemaName: string read FSchemaName write SetSchemaName;
  end;

  { TdxServerModeDBXDataSourceHelper }

  TdxServerModeDBXDataSourceHelper = class(TdxServerModeDataSourceCustomHelper)
  protected
    function GetDataSetClass: TdxDataSetClass; override;
    function GetOptionsClass: TdxServerModeDataSourceOptionsClass; override;
    function GetQueryBuilderClass: TdxServerModeCustomSQLQueryBuilderClass; override;

    procedure CheckParamValue(var AParam: TdxServerModeParam); override;
    procedure DoInitializeDataSet(ADataSet: TDataSet); override;
    procedure DoSynchronizeDataSetConnection(ADataSet: TDataSet); override;
    procedure PrepareDatasetForIterate(ADataSet: TDataSet); override;
  public
    procedure PopulateSchemaNames(AList: TStrings);
  end;

  { TdxServerModeDBXDataSource }

  TdxServerModeDBXDataSource = class(TdxServerModeDataSource)
  private
    function GetConnection: TSQLConnection;
    function GetOptions: TdxServerModeDBXDataSourceOptions;
    function GetHelper: TdxServerModeDBXDataSourceHelper;
    procedure SetConnection(Value: TSQLConnection);
    procedure SetOptions(const Value: TdxServerModeDBXDataSourceOptions);
  protected
    procedure DoPopulateTableNames(AList: TStrings); override;
    procedure DoPopulateViewNames(AList: TStrings); override;
    function GetHelperClass: TdxServerModeDataSourceCustomHelperClass; override;

    property Helper: TdxServerModeDBXDataSourceHelper read GetHelper;
  public
    procedure PopulateSchemaNames(AList: TStrings);
  published
    property Connection: TSQLConnection read GetConnection write SetConnection;
    property Options: TdxServerModeDBXDataSourceOptions read GetOptions write SetOptions;
  end;

  { TdxServerModeDBXQueryDataSource }

  TdxServerModeDBXQueryDataSource = class(TdxServerModeQueryDataSource)
  private
    function GetConnection: TSQLConnection;
    function GetOptions: TdxServerModeDBXDataSourceOptions;
    function GetHelper: TdxServerModeDBXDataSourceHelper;
    procedure SetConnection(Value: TSQLConnection);
    procedure SetOptions(const Value: TdxServerModeDBXDataSourceOptions);
  protected
    function GetHelperClass: TdxServerModeDataSourceCustomHelperClass; override;

    property Helper: TdxServerModeDBXDataSourceHelper read GetHelper;
  public
    procedure PopulateSchemaNames(AList: TStrings);
  published
    property Connection: TSQLConnection read GetConnection write SetConnection;
    property Options: TdxServerModeDBXDataSourceOptions read GetOptions write SetOptions;
  end;

implementation

uses
  SqlTimSt;

{ TdxServerModeDBXSQLQueryBuilder }

function TdxServerModeDBXSQLQueryBuilder.CanUseParams: Boolean;
begin
  Result := inherited CanUseParams;
end;

{ TdxServerModeDBXDataSource }

procedure TdxServerModeDBXDataSource.PopulateSchemaNames(AList: TStrings);
begin
  Helper.PopulateSchemaNames(AList);
end;

procedure TdxServerModeDBXDataSource.DoPopulateTableNames(AList: TStrings);
begin
  Connection.GetTableNames(AList, Options.SchemaName);
end;

procedure TdxServerModeDBXDataSource.DoPopulateViewNames(AList: TStrings);
var
  ATableScope: TTableScopes;
begin
  ATableScope := Connection.TableScope;
  try
    Connection.TableScope := [tsView];
    Connection.GetTableNames(AList, Options.SchemaName);
  finally
    Connection.TableScope := ATableScope;
  end;
end;

function TdxServerModeDBXDataSource.GetHelperClass: TdxServerModeDataSourceCustomHelperClass;
begin
  Result := TdxServerModeDBXDataSourceHelper;
end;

function TdxServerModeDBXDataSource.GetOptions: TdxServerModeDBXDataSourceOptions;
begin
  Result := inherited Options as TdxServerModeDBXDataSourceOptions;
end;

function TdxServerModeDBXDataSource.GetHelper: TdxServerModeDBXDataSourceHelper;
begin
  Result := TdxServerModeDBXDataSourceHelper(inherited Helper);
end;

function TdxServerModeDBXDataSource.GetConnection: TSQLConnection;
begin
  Result := TSQLConnection(inherited Connection);
end;

procedure TdxServerModeDBXDataSource.SetConnection(Value: TSQLConnection);
begin
  inherited Connection := Value;
end;

procedure TdxServerModeDBXDataSource.SetOptions(const Value: TdxServerModeDBXDataSourceOptions);
begin
  inherited Options := Value;
end;

{ TdxServerModeDBXDataSourceOptions }

procedure TdxServerModeDBXDataSourceOptions.Assign(Source: TPersistent);
begin
  if Source is TdxServerModeDBXDataSourceOptions then
    SchemaName := TdxServerModeDBXDataSourceOptions(Source).SchemaName;
  inherited Assign(Source);
end;

function TdxServerModeDBXDataSourceOptions.GetSchemaName: string;
begin
  Result := FSchemaName;
end;

procedure TdxServerModeDBXDataSourceOptions.SetSchemaName(const Value: string);
begin
  if SchemaName <> Value then
  begin
    FSchemaName := Value;
    Changed;
  end;
end;

{ TdxServerModeDBXDataSourceHelper }

procedure TdxServerModeDBXDataSourceHelper.PopulateSchemaNames(AList: TStrings);
begin
  if not IsConnected then
    Exit;
  TSQLConnection(Connection).GetSchemaNames(AList);
end;

function TdxServerModeDBXDataSourceHelper.GetDataSetClass: TdxDataSetClass;
begin
  Result := TSQLDataSet;
end;

function TdxServerModeDBXDataSourceHelper.GetOptionsClass: TdxServerModeDataSourceOptionsClass;
begin
  Result := TdxServerModeDBXDataSourceOptions;
end;

function TdxServerModeDBXDataSourceHelper.GetQueryBuilderClass: TdxServerModeCustomSQLQueryBuilderClass;
begin
  Result := TdxServerModeDBXSQLQueryBuilder;
end;

procedure TdxServerModeDBXDataSourceHelper.CheckParamValue(var AParam: TdxServerModeParam);
begin
  inherited;
  if AParam.DataType = ftDateTime then
    AParam.AsSQLTimeStamp := VarToSQLTimeStamp(AParam.Value);
end;

procedure TdxServerModeDBXDataSourceHelper.DoInitializeDataSet(ADataSet: TDataSet);
begin
  TSQLDataSet(ADataSet).SchemaName := TdxServerModeDBXDataSourceOptions(DataSource.Options).SchemaName;
  TSQLDataSet(ADataSet).CommandText := GetFieldsRetrieveQuery;
end;

procedure TdxServerModeDBXDataSourceHelper.DoSynchronizeDataSetConnection(ADataSet: TDataSet);
begin
  TSQLDataSet(ADataSet).SQLConnection := TSQLConnection(Connection);
end;

procedure TdxServerModeDBXDataSourceHelper.PrepareDatasetForIterate(ADataSet: TDataSet);
begin
  inherited PrepareDatasetForIterate(ADataSet);
  TSQLDataSet(ADataSet).GetMetadata := False;
end;

{ TdxServerModeDBXQueryDataSource }

procedure TdxServerModeDBXQueryDataSource.PopulateSchemaNames(AList: TStrings);
begin
  Helper.PopulateSchemaNames(AList);
end;

function TdxServerModeDBXQueryDataSource.GetHelperClass: TdxServerModeDataSourceCustomHelperClass;
begin
  Result := TdxServerModeDBXDataSourceHelper;
end;

function TdxServerModeDBXQueryDataSource.GetOptions: TdxServerModeDBXDataSourceOptions;
begin
  Result := inherited Options as TdxServerModeDBXDataSourceOptions;
end;

function TdxServerModeDBXQueryDataSource.GetHelper: TdxServerModeDBXDataSourceHelper;
begin
  Result := TdxServerModeDBXDataSourceHelper(inherited Helper);
end;

function TdxServerModeDBXQueryDataSource.GetConnection: TSQLConnection;
begin
  Result := TSQLConnection(inherited Connection);
end;

procedure TdxServerModeDBXQueryDataSource.SetConnection(Value: TSQLConnection);
begin
  inherited Connection := Value;
end;

procedure TdxServerModeDBXQueryDataSource.SetOptions(const Value: TdxServerModeDBXDataSourceOptions);
begin
  inherited Options := Value;
end;

end.

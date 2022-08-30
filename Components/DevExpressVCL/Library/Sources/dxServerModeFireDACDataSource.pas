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

unit dxServerModeFireDACDataSource;

{$I cxVer.inc}

interface

uses
  Variants, SysUtils, Classes, DB,
{$IFDEF DELPHI19}
  FireDAC.Comp.Client,
{$ELSE}
  uADCompClient,
{$ENDIF}
  dxServerModeClasses, dxServerModeData, cxFilter;

type

  TdxServerModeFireDACDataSource = class;

  { TdxServerModeFireDACSQLQueryBuilder }

  TdxServerModeFireDACSQLQueryBuilder = class(TdxServerModeCustomSQLQueryBuilder);

  { TdxServerModeFireDACDataSourceOptions }

  TdxServerModeFireDACDataSourceOptions = class(TdxServerModeDataSourceOptions)
  private
    FSchemaName: string;
    FCatalogName: string;
    procedure SetSchemaName(const Value: string);
    procedure SetCatalogName(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    function GetSchemaName: string; override;
  published
    property CatalogName: string read FCatalogName write SetCatalogName;
    property SchemaName: string read FSchemaName write SetSchemaName;
  end;

  { TdxServerModeFireDACQueryDataSourceOptions }

  TdxServerModeFireDACQueryDataSourceOptions = class(TdxServerModeDataSourceOptions);

  { TdxServerModeFireDACDataSourceCustomHelper }

  TdxServerModeFireDACDataSourceCustomHelper = class(TdxServerModeDataSourceCustomHelper)
  protected
    function GetDataSetClass: TdxDataSetClass; override;
    function GetQueryBuilderClass: TdxServerModeCustomSQLQueryBuilderClass; override;

    procedure CheckParamValue(var AParam: TdxServerModeParam); override;
    procedure DoExecuteCommand(AResultSet: PDataSet; const ACommand: string; AParams: TParams; AParamCheck: Boolean); override;
    procedure DoInitializeDataSet(ADataSet: TDataSet); override;
    procedure DoSynchronizeDataSetConnection(ADataSet: TDataSet); override;
  end;

  { TdxServerModeFireDACDataSourceHelper }

  TdxServerModeFireDACDataSourceHelper = class(TdxServerModeFireDACDataSourceCustomHelper)
  private
    function GetOptions: TdxServerModeFireDACDataSourceOptions;
  protected
    function GetOptionsClass: TdxServerModeDataSourceOptionsClass; override;

    property Options: TdxServerModeFireDACDataSourceOptions read GetOptions;
  public
    procedure PopulateCatalogNames(AList: TStrings);
    procedure PopulateSchemaNames(AList: TStrings);
  end;

  { TdxServerModeFireDACQueryDataSourceHelper }

  TdxServerModeFireDACQueryDataSourceHelper = class(TdxServerModeFireDACDataSourceCustomHelper)
  protected
    function GetOptionsClass: TdxServerModeDataSourceOptionsClass; override;
  end;


  { TdxServerModeFireDACDataSource }

  TdxServerModeFireDACDataSource = class(TdxServerModeDataSource)
  private
    FFullTableName: string;
    function GetConnection: {$IFDEF DELPHI19}TFDConnection{$ELSE}TADConnection{$ENDIF};
    function GetOptions: TdxServerModeFireDACDataSourceOptions;
    function GetHelper: TdxServerModeFireDACDataSourceHelper;
    procedure SetConnection(Value: {$IFDEF DELPHI19}TFDConnection{$ELSE}TADConnection{$ENDIF});
    procedure SetOptions(const Value: TdxServerModeFireDACDataSourceOptions);
    procedure UpdateFullTableName;
  protected
    procedure DoPopulateKeyFields(AList: TStrings); override;
    procedure DoPopulateTableNames(AList: TStrings); override;
    procedure DoPopulateViewNames(AList: TStrings); override;
    function GetFromSQLString: string; override;
    function GetHelperClass: TdxServerModeDataSourceCustomHelperClass; override;
    procedure CheckSettings; override;

    property Helper: TdxServerModeFireDACDataSourceHelper read GetHelper;
  public
    procedure PopulateCatalogNames(AList: TStrings);
    procedure PopulateSchemaNames(AList: TStrings);
  published
    property Connection: {$IFDEF DELPHI19}TFDConnection{$ELSE}TADConnection{$ENDIF} read GetConnection write SetConnection;
    property Options: TdxServerModeFireDACDataSourceOptions read GetOptions write SetOptions;
  end;

  { TdxServerModeFireDACQueryDataSource }

  TdxServerModeFireDACQueryDataSource = class(TdxServerModeQueryDataSource)
  private
    function GetConnection: {$IFDEF DELPHI19}TFDConnection{$ELSE}TADConnection{$ENDIF};
    function GetOptions: TdxServerModeFireDACQueryDataSourceOptions;
    procedure SetConnection(Value: {$IFDEF DELPHI19}TFDConnection{$ELSE}TADConnection{$ENDIF});
    procedure SetOptions(const Value: TdxServerModeFireDACQueryDataSourceOptions);
  protected
    function GetHelperClass: TdxServerModeDataSourceCustomHelperClass; override;
  published
    property Connection: {$IFDEF DELPHI19}TFDConnection{$ELSE}TADConnection{$ENDIF} read GetConnection write SetConnection;
    property Options: TdxServerModeFireDACQueryDataSourceOptions read GetOptions write SetOptions;
  end;

implementation

uses
{$IFDEF DELPHI19}
  FireDAC.Phys.Intf,
{$ELSE}
  uADPhysIntf,
{$ENDIF}
  SqlTimSt;

{ TdxServerModeFireDACDataSource }

procedure TdxServerModeFireDACDataSource.PopulateCatalogNames(AList: TStrings);
begin
  Helper.PopulateCatalogNames(AList);
end;

procedure TdxServerModeFireDACDataSource.PopulateSchemaNames(AList: TStrings);
begin
  Helper.PopulateSchemaNames(AList);
end;

procedure TdxServerModeFireDACDataSource.DoPopulateKeyFields(AList: TStrings);
begin
  Connection.GetKeyFieldNames(Options.CatalogName, Options.SchemaName, TableName, '', AList);
end;

procedure TdxServerModeFireDACDataSource.DoPopulateTableNames(AList: TStrings);
begin
  Connection.GetTableNames(Options.CatalogName, Options.SchemaName, '', AList, [osMy], [tkTable, tkView]);
end;

procedure TdxServerModeFireDACDataSource.DoPopulateViewNames(AList: TStrings);
begin
  Connection.GetTableNames(Options.CatalogName, Options.SchemaName, '', AList, [osMy], [tkView]);
end;

function TdxServerModeFireDACDataSource.GetHelperClass: TdxServerModeDataSourceCustomHelperClass;
begin
  Result := TdxServerModeFireDACDataSourceHelper;
end;

procedure TdxServerModeFireDACDataSource.CheckSettings;
begin
  inherited CheckSettings;
  UpdateFullTableName;
end;

function TdxServerModeFireDACDataSource.GetOptions: TdxServerModeFireDACDataSourceOptions;
begin
  Result := inherited Options as TdxServerModeFireDACDataSourceOptions;
end;

function TdxServerModeFireDACDataSource.GetHelper: TdxServerModeFireDACDataSourceHelper;
begin
  Result := TdxServerModeFireDACDataSourceHelper(inherited Helper);
end;

function TdxServerModeFireDACDataSource.GetConnection: {$IFDEF DELPHI19}TFDConnection{$ELSE}TADConnection{$ENDIF};
begin
{$IFDEF DELPHI19}
  Result := TFDConnection(inherited Connection);
{$ELSE}
  Result := TADConnection(inherited Connection);
{$ENDIF}
end;

function TdxServerModeFireDACDataSource.GetFromSQLString: string;
begin
  Result := FFullTableName;
end;

procedure TdxServerModeFireDACDataSource.SetConnection(Value: {$IFDEF DELPHI19}TFDConnection{$ELSE}TADConnection{$ENDIF});
begin
  inherited Connection := Value;
end;

procedure TdxServerModeFireDACDataSource.SetOptions(const Value: TdxServerModeFireDACDataSourceOptions);
begin
  inherited Options := Value;
end;

procedure TdxServerModeFireDACDataSource.UpdateFullTableName;
var
  ACatalogName, ASchemaName, ABaseObjectName, AObjectName: string;
begin
  Connection.DecodeObjectName(TableName, ACatalogName, ASchemaName, ABaseObjectName, AObjectName);
  if ACatalogName = '' then
    ACatalogName := Options.CatalogName;
  if ASchemaName = '' then
    ASchemaName := Options.SchemaName;
  FFullTableName := Connection.EncodeObjectName(ACatalogName, ASchemaName, ABaseObjectName, AObjectName);
end;

{ TdxServerModeFireDACDataSourceOptions }

procedure TdxServerModeFireDACDataSourceOptions.Assign(Source: TPersistent);
begin
  if Source is TdxServerModeFireDACDataSourceOptions then
  begin
    CatalogName := TdxServerModeFireDACDataSourceOptions(Source).CatalogName;
    SchemaName := TdxServerModeFireDACDataSourceOptions(Source).SchemaName;
  end;
  inherited Assign(Source);
end;

function TdxServerModeFireDACDataSourceOptions.GetSchemaName: string;
begin
  Result := FSchemaName;
end;

procedure TdxServerModeFireDACDataSourceOptions.SetCatalogName(const Value: string);
begin
  if CatalogName <> Value then
  begin
    CheckInactive;
    FCatalogName := Value;
    Changed([ctConnection]);
  end;
end;

procedure TdxServerModeFireDACDataSourceOptions.SetSchemaName(const Value: string);
begin
  if SchemaName <> Value then
  begin
    CheckInactive;
    FSchemaName := Value;
    Changed([ctConnection]);
  end;
end;

{ TdxServerModeFireDACDataSourceCustomHelper }

function TdxServerModeFireDACDataSourceCustomHelper.GetDataSetClass: TdxDataSetClass;
begin
{$IFDEF DELPHI19}
  Result := TFDCustomQuery;
{$ELSE}
  Result := TADCustomQuery;
{$ENDIF}
end;

function TdxServerModeFireDACDataSourceCustomHelper.GetQueryBuilderClass: TdxServerModeCustomSQLQueryBuilderClass;
begin
  Result := TdxServerModeFireDACSQLQueryBuilder;
end;

procedure TdxServerModeFireDACDataSourceCustomHelper.DoExecuteCommand(
  AResultSet: PDataSet; const ACommand: string; AParams: TParams;
  AParamCheck: Boolean);
var
  AIntf: IdxProviderSupport;
  ASendParams: TParams;
  AOwnParams: Boolean;
{$IFDEF DELPHI17}
  ADataSet: TDataSet;
{$ENDIF}
begin
  Supports(DataSet, IdxProviderSupport, AIntf);
  AOwnParams := AParams = nil;
  if AOwnParams then
    ASendParams := TParams.Create(nil)
  else
    ASendParams := AParams;
  try
  {$IFDEF DELPHI17}
    if AResultSet = nil then
      AIntf.PSExecuteStatement(ACommand, ASendParams)
    else
    begin
      AIntf.PSExecuteStatement(ACommand, ASendParams, ADataSet);
      AResultSet^ := ADataSet;
    end;
  {$ELSE}
    AIntf.PSExecuteStatement(ACommand, ASendParams, AResultSet);
  {$ENDIF}
  finally
    if AOwnParams then
      ASendParams.Free;
  end;
end;

procedure TdxServerModeFireDACDataSourceCustomHelper.CheckParamValue(var AParam: TdxServerModeParam);
begin
  inherited CheckParamValue(AParam);
  if (AParam.DataType = ftString) and (AParam.Value = '') then
  begin
    AParam.DataType := ftWideString;
    AParam.Value := '';
  end;
end;

procedure TdxServerModeFireDACDataSourceCustomHelper.DoInitializeDataSet(ADataSet: TDataSet);
begin
{$IFDEF DELPHI19}
  TFDCustomQuery(ADataSet).SQL.Text := GetFieldsRetrieveQuery;
  TFDCustomQuery(ADataSet).FormatOptions.StrsEmpty2Null := False;
{$ELSE}
  TADCustomQuery(ADataSet).SQL.Text := GetFieldsRetrieveQuery;
  TADCustomQuery(ADataSet).FormatOptions.StrsEmpty2Null := False;
{$ENDIF}
end;

procedure TdxServerModeFireDACDataSourceCustomHelper.DoSynchronizeDataSetConnection(ADataSet: TDataSet);
begin
{$IFDEF DELPHI19}
  TFDCustomQuery(ADataSet).Connection := TFDConnection(Connection);
{$ELSE}
  TADCustomQuery(ADataSet).Connection := TADConnection(Connection);
{$ENDIF}
end;

{ TdxServerModeFireDACDataSourceHelper }

procedure TdxServerModeFireDACDataSourceHelper.PopulateCatalogNames(AList: TStrings);
begin
  if not IsConnected then
    Exit;
{$IFDEF DELPHI19}
  TFDConnection(Connection).GetCatalogNames('', AList);
{$ELSE}
  TADConnection(Connection).GetCatalogNames('', AList);
{$ENDIF}
end;

procedure TdxServerModeFireDACDataSourceHelper.PopulateSchemaNames(AList: TStrings);
begin
  if not IsConnected then
    Exit;
{$IFDEF DELPHI19}
  TFDConnection(Connection).GetSchemaNames(Options.CatalogName, '', AList);
{$ELSE}
  TADConnection(Connection).GetSchemaNames(Options.CatalogName, '', AList);
{$ENDIF}
end;

function TdxServerModeFireDACDataSourceHelper.GetOptionsClass: TdxServerModeDataSourceOptionsClass;
begin
  Result := TdxServerModeFireDACDataSourceOptions;
end;

function TdxServerModeFireDACDataSourceHelper.GetOptions: TdxServerModeFireDACDataSourceOptions;
begin
  Result := TdxServerModeFireDACDataSourceOptions(DataSource.Options);
end;

{ TdxServerModeFireDACQueryDataSourceHelper }

function TdxServerModeFireDACQueryDataSourceHelper.GetOptionsClass: TdxServerModeDataSourceOptionsClass;
begin
  Result := TdxServerModeFireDACQueryDataSourceOptions;
end;

{ TdxServerModeFireDACQueryDataSource }

function TdxServerModeFireDACQueryDataSource.GetHelperClass: TdxServerModeDataSourceCustomHelperClass;
begin
  Result := TdxServerModeFireDACQueryDataSourceHelper;
end;

function TdxServerModeFireDACQueryDataSource.GetOptions: TdxServerModeFireDACQueryDataSourceOptions;
begin
  Result := TdxServerModeFireDACQueryDataSourceOptions(inherited Options);
end;

function TdxServerModeFireDACQueryDataSource.GetConnection: {$IFDEF DELPHI19}TFDConnection{$ELSE}TADConnection{$ENDIF};
begin
{$IFDEF DELPHI19}
  Result := TFDConnection(inherited Connection);
{$ELSE}
  Result := TADConnection(inherited Connection);
{$ENDIF}
end;

procedure TdxServerModeFireDACQueryDataSource.SetConnection(Value: {$IFDEF DELPHI19}TFDConnection{$ELSE}TADConnection{$ENDIF});
begin
  inherited Connection := Value;
end;

procedure TdxServerModeFireDACQueryDataSource.SetOptions(const Value: TdxServerModeFireDACQueryDataSourceOptions);
begin
  inherited Options := Value;
end;

end.

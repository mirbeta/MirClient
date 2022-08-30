{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.DocumentModel.MailMerge;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, DB,
  dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxGenerics;

type

  TdxMergeMode = TdxRichEditMergeMode;

  TdxMergeRecords = TdxRichEditMergeRecords;

  TdxMailMergeFieldType = (
    Null,
    DbColumn
  );

  TdxMailMergeDestination = (
    EMail,
    Fax,
    NewDocument,
    Printer
  );

  TdxMailMergeDataType = (
    Native,
    Odbc,
    Query,
    Spreadsheet,
    TextFile
  );

  TdxMailMergeSourceType = (
    AddressBook,
    Database,
    Document1,
    Document2,
    EMailProgram,
    Legacy,
    Master,
    Native,
    Text
  );

  { IdxDataSourceContainerOptions }

  IdxDataSourceContainerOptions = interface
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);

    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  { IdxMailMergeOptions }

  IdxMailMergeOptions = interface(IdxDataSourceContainerOptions)
    function GetCopyTemplateStyles: Boolean;
    function GetHeaderFooterLinkToPrevious: Boolean;
    function GetMergeMode: TdxMergeMode;
    function GetMergeRecords: TdxMergeRecords;
    procedure SetCopyTemplateStyles(const Value: Boolean);
    procedure SetHeaderFooterLinkToPrevious(const Value: Boolean);
    procedure SetMergeMode(const Value: TdxMergeMode);
    procedure SetMergeRecords(const MergeRecords: TdxMergeRecords);

    property CopyTemplateStyles: Boolean read GetCopyTemplateStyles write SetCopyTemplateStyles;
    property HeaderFooterLinkToPrevious: Boolean read GetHeaderFooterLinkToPrevious write SetHeaderFooterLinkToPrevious;
    property MergeMode: TdxMergeMode read GetMergeMode write SetMergeMode;
    property MergeRecords: TdxMergeRecords read GetMergeRecords write SetMergeRecords;
  end;

  { TdxMergeFieldName }

  TdxMergeFieldName = class(TcxIUnknownObject, IdxRichEditMergeFieldName, IdxComparable<IdxRichEditMergeFieldName>)
  strict private
    FName: string;
    FDisplayName: string;
    // IdxRichEditMergeFieldName
    function GetDisplayName: string;
    function GetName: string;
    procedure SetDisplayName(const Value: string);
    procedure SetName(const Value: string);
  public
    constructor Create(const AName: string); overload;
    constructor Create(const AName: string; const ADisplayName: string); overload;
    function ToString: string; override;
    function CompareTo(const AOther: IdxRichEditMergeFieldName): Integer;

    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
  end;

  { TdxFieldMapData }

  TdxFieldMapData = class
  strict private
    FColumnIndex: Integer;
    FDynamicAddress: Boolean;
    FMappedName: string;
    FColumnName: string;
    FFieldType: TdxMailMergeFieldType;
    FMergeFieldNameLanguageId: Integer;
  private
    FChanged: TdxEventHandler;
    procedure SetColumnIndex(const Value: Integer);
    procedure SetColumnName(const Value: string);
    procedure SetDynamicAddress(const Value: Boolean);
    procedure SetFieldType(const Value: TdxMailMergeFieldType);
    procedure SetMappedName(const Value: string);
    procedure SetMergeFieldNameLanguageId(const Value: Integer);
  protected
    procedure DoChanged;
  public

    property ColumnIndex: Integer read FColumnIndex write SetColumnIndex;
    property ColumnName: string read FColumnName write SetColumnName;
    property DynamicAddress: Boolean read FDynamicAddress write SetDynamicAddress;
    property FieldType: TdxMailMergeFieldType read FFieldType write SetFieldType;
    property MappedName: string read FMappedName write SetMappedName;
    property MergeFieldNameLanguageId: Integer read FMergeFieldNameLanguageId write SetMergeFieldNameLanguageId;

    property Changed: TdxEventHandler read FChanged;
  end;

  { TdxDataSourceObjectProperties }

  TdxDataSourceObjectProperties = class
  strict private
    FMapColumnName: TdxNamedOrdinalDictionary<TdxFieldMapData>;
    FMapFieldMappedName: TdxNamedOrdinalDictionary<TdxFieldMapData>;
    FColumnDelimiter: Char;
    FFirstRowHeader: Boolean;
    FFieldsMapData: TdxNotificationCollection<TdxFieldMapData>;
    FTableName: string;
    FDataSourceType: TdxMailMergeSourceType;
    FUdlConnectionString: string;
  private
    FColumnDelimiterChanged: TdxEventHandler;
    FFieldsMapDataChanged: TdxEventHandler;
    FUdlConnectionStringChanged: TdxEventHandler;
    FDataSourceTypeChanged: TdxEventHandler;
    FFirstRowHeaderChanged: TdxEventHandler;
    FTableNameChanged: TdxEventHandler;
    procedure SetColumnDelimiter(const Value: Char);
    procedure SetDataSourceType(const Value: TdxMailMergeSourceType);
    procedure SetFirstRowHeader(const Value: Boolean);
    procedure SetTableName(const Value: string);
    procedure SetUdlConnectionString(const Value: string);
  protected
    procedure DoColumnDelimiterChanged; virtual;
    procedure DoFirstRowHeaderChanged; virtual;
    procedure DoTableNameChanged; virtual;
    procedure DoDataSourceTypeChanged; virtual;
    procedure DoUdlConnectionStringChanged; virtual;
    procedure DoFieldsMapDataChanged; virtual;

    procedure InternalFieldsMapDataChanged(Sender: TObject);

    procedure SubscribeFieldsMapDataEvents; virtual;
    procedure UnsubscribeFieldsMapDataEvents; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function FindMapDataByColumnName(const AColumnName: string): TdxFieldMapData; virtual;
    function FindMapDataByMapName(const AMappedFieldName: string): TdxFieldMapData; virtual;

    property ColumnDelimiter: Char read FColumnDelimiter write SetColumnDelimiter;
    property DataSourceType: TdxMailMergeSourceType read FDataSourceType write SetDataSourceType;
    property FirstRowHeader: Boolean read FFirstRowHeader write SetFirstRowHeader;
    property FieldsMapData: TdxNotificationCollection<TdxFieldMapData> read FFieldsMapData;
    property TableName: string read FTableName write SetTableName;
    property UdlConnectionString: string read FUdlConnectionString write SetUdlConnectionString;

    property ColumnDelimiterChanged: TdxEventHandler read FColumnDelimiterChanged;
    property FirstRowHeaderChanged: TdxEventHandler read FFirstRowHeaderChanged;
    property TableNameChanged: TdxEventHandler  read FTableNameChanged;
    property DataSourceTypeChanged: TdxEventHandler  read FDataSourceTypeChanged;
    property UdlConnectionStringChanged: TdxEventHandler  read FUdlConnectionStringChanged;
    property FieldsMapDataChanged: TdxEventHandler read FFieldsMapDataChanged;
  end;

  { TdxMailMergeProperties }

  TdxMailMergeProperties = class
  private
    FDestination: TdxMailMergeDestination;
    FDataSource: string;
    FConnectionString: string;
    FQuery: string;
    FLeaveBlankLines: Boolean;
    FDataType: TdxMailMergeDataType;
    FViewMergedData: Boolean;
    FEMailAddressColumnName: string;
    FDataSourceObjectProperties: TdxDataSourceObjectProperties;
    FDestinationChanged: TdxEventHandler;
    FDataSourceChanged: TdxEventHandler;
    FConnectionStringChanged: TdxEventHandler;
    FQueryChanged: TdxEventHandler;
    FLeaveBlankLinesChanged: TdxEventHandler;
    FDataTypeChanged: TdxEventHandler;
    FViewMergedDataChanged: TdxEventHandler;
    FEMailAddressColumnNameChanged: TdxEventHandler;
    procedure SetConnectionString(const Value: string);
    procedure SetDataSource(const Value: string);
    procedure SetDataType(const Value: TdxMailMergeDataType);
    procedure SetDestination(const Value: TdxMailMergeDestination);
    procedure SetEMailAddressColumnName(const Value: string);
    procedure SetLeaveBlankLines(const Value: Boolean);
    procedure SetQuery(const Value: string);
    procedure SetViewMergedData(const Value: Boolean);
  protected
    procedure DoEMailAddressColumnNameChanged; virtual;
    procedure DoConnectionStringChanged; virtual;
    procedure DoDataSourceChanged; virtual;
    procedure DoDataTypeChanged; virtual;
    procedure DoDestinationChanged; virtual;
    procedure DoLeaveBlankLinesChanged; virtual;
    procedure DoQueryChanged; virtual;
    procedure DoViewMergedDataChanged; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property ConnectionString: string read FConnectionString write SetConnectionString;
    property DataSource: string read FDataSource write SetDataSource;
    property DataSourceObjectProperties: TdxDataSourceObjectProperties read FDataSourceObjectProperties;
    property DataType: TdxMailMergeDataType read FDataType write SetDataType;
    property Destination: TdxMailMergeDestination read FDestination write SetDestination;
    property EMailAddressColumnName: string read FEMailAddressColumnName write SetEMailAddressColumnName;
    property LeaveBlankLines: Boolean read FLeaveBlankLines write SetLeaveBlankLines;
    property Query: string read FQuery write SetQuery;
    property ViewMergedData: Boolean read FViewMergedData write SetViewMergedData;

    property EMailAddressColumnNameChanged: TdxEventHandler read FEMailAddressColumnNameChanged;
    property ConnectionStringChanged: TdxEventHandler read FConnectionStringChanged;
    property DataSourceChanged: TdxEventHandler read FDataSourceChanged;
    property DataTypeChanged: TdxEventHandler read FDataTypeChanged;
    property DestinationChanged: TdxEventHandler read FDestinationChanged;
    property LeaveBlankLinesChanged: TdxEventHandler read FLeaveBlankLinesChanged;
    property QueryChanged: TdxEventHandler read FQueryChanged;
    property ViewMergedDataChanged: TdxEventHandler read FViewMergedDataChanged;
  end;

  { TdxMailMergeOptions }

  TdxMailMergeOptions = class(TcxIUnknownObject, IdxMailMergeOptions, IdxDataSourceContainerOptions)
  strict private
    FMergeMode: TdxMergeMode;
    FDataSource: TDataSource;
    FCopyTemplateStyles: Boolean;
    FHeaderFooterLinkToPrevious: Boolean;
    FMergeRecords: TdxMergeRecords;

    //IdxDataSourceContainerOptions
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    //IdxMailMergeOptions
    function GetCopyTemplateStyles: Boolean;
    function GetHeaderFooterLinkToPrevious: Boolean;
    function GetMergeMode: TdxMergeMode;
    function GetMergeRecords: TdxMergeRecords;
    procedure SetCopyTemplateStyles(const Value: Boolean);
    procedure SetHeaderFooterLinkToPrevious(const Value: Boolean);
    procedure SetMergeMode(const Value: TdxMergeMode);
    procedure SetMergeRecords(const Value: TdxMergeRecords);
  public
    property MergeMode: TdxMergeMode read GetMergeMode write SetMergeMode;
    //IdxMailMergeOptions
    property CopyTemplateStyles: Boolean read GetCopyTemplateStyles write SetCopyTemplateStyles;
    property HeaderFooterLinkToPrevious: Boolean read GetHeaderFooterLinkToPrevious write SetHeaderFooterLinkToPrevious;
    property MergeRecords: TdxMergeRecords read GetMergeRecords write SetMergeRecords;
    //IdxDataSourceContainerOptions
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

implementation

{ TdxMergeFieldName }

constructor TdxMergeFieldName.Create(const AName: string; const ADisplayName: string);
begin
  inherited Create;
  FName := AName;
  FDisplayName := ADisplayName;
end;

constructor TdxMergeFieldName.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FDisplayName := AName;
end;

function TdxMergeFieldName.ToString: string;
begin
  Result := DisplayName;
end;

function TdxMergeFieldName.CompareTo(const AOther: IdxRichEditMergeFieldName): Integer;
begin
  Result := CompareText(DisplayName, AOther.DisplayName);
end;

function TdxMergeFieldName.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TdxMergeFieldName.GetName: string;
begin
  Result := FName;
end;

procedure TdxMergeFieldName.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TdxMergeFieldName.SetName(const Value: string);
begin
  FName := Value;
end;

{ TdxFieldMapData }

procedure TdxFieldMapData.DoChanged;
begin
  if not FChanged.Empty then
    FChanged.Invoke(Self, nil);
end;

procedure TdxFieldMapData.SetColumnIndex(const Value: Integer);
begin
  if FColumnIndex = Value then
    Exit;
  FColumnIndex := Value;
  DoChanged;
end;

procedure TdxFieldMapData.SetColumnName(const Value: string);
begin
  if FColumnName = Value then
    Exit;
  FColumnName := Value;
  DoChanged;
end;

procedure TdxFieldMapData.SetDynamicAddress(const Value: Boolean);
begin
  if FDynamicAddress = Value then
    Exit;
  FDynamicAddress := Value;
  DoChanged;
end;

procedure TdxFieldMapData.SetFieldType(const Value: TdxMailMergeFieldType);
begin
  if FFieldType = Value then
    Exit;
  FFieldType := Value;
  DoChanged;
end;

procedure TdxFieldMapData.SetMappedName(const Value: string);
begin
  if FMappedName = Value then
    Exit;
  FMappedName := Value;
  DoChanged;
end;

procedure TdxFieldMapData.SetMergeFieldNameLanguageId(const Value: Integer);
begin
  if FMergeFieldNameLanguageId = Value then
    Exit;
  FMergeFieldNameLanguageId := Value;
  DoChanged;
end;

{ TdxDataSourceObjectProperties }

constructor TdxDataSourceObjectProperties.Create;
begin
  inherited Create;
  FFieldsMapData := TdxNotificationObjectCollection<TdxFieldMapData>.Create;
  FMapColumnName := TdxNamedOrdinalDictionary<TdxFieldMapData>.Create;
  FMapFieldMappedName := TdxNamedOrdinalDictionary<TdxFieldMapData>.Create;
  SubscribeFieldsMapDataEvents;
end;

destructor TdxDataSourceObjectProperties.Destroy;
begin
  FreeAndNil(FFieldsMapData);
  FreeAndNil(FMapColumnName);
  FreeAndNil(FMapFieldMappedName);
  inherited Destroy;
end;

procedure TdxDataSourceObjectProperties.DoColumnDelimiterChanged;
begin
  if not FColumnDelimiterChanged.Empty then
    FColumnDelimiterChanged.Invoke(Self, nil);
end;

procedure TdxDataSourceObjectProperties.DoDataSourceTypeChanged;
begin
  if not FDataSourceTypeChanged.Empty then
    FDataSourceTypeChanged.Invoke(Self, nil);
end;

procedure TdxDataSourceObjectProperties.DoFieldsMapDataChanged;
begin
  if not FFieldsMapDataChanged.Empty then
    FFieldsMapDataChanged.Invoke(Self, nil);
end;

procedure TdxDataSourceObjectProperties.DoFirstRowHeaderChanged;
begin
  if not FFirstRowHeaderChanged.Empty then
    FFirstRowHeaderChanged.Invoke(Self, nil);
end;

procedure TdxDataSourceObjectProperties.DoTableNameChanged;
begin
  if not FTableNameChanged.Empty then
    FTableNameChanged.Invoke(Self, nil);
end;

procedure TdxDataSourceObjectProperties.DoUdlConnectionStringChanged;
begin
  if not FUdlConnectionStringChanged.Empty then
    FUdlConnectionStringChanged.Invoke(Self, nil);
end;

function TdxDataSourceObjectProperties.FindMapDataByColumnName(const AColumnName: string): TdxFieldMapData;
begin
  if not FMapColumnName.TryGetValue(AColumnName, Result) then
    Result := nil;
end;

function TdxDataSourceObjectProperties.FindMapDataByMapName(const AMappedFieldName: string): TdxFieldMapData;
begin
  FMapFieldMappedName.TryGetValue(AMappedFieldName, Result);
end;

procedure TdxDataSourceObjectProperties.InternalFieldsMapDataChanged(Sender: TObject);
var
  ACount: Integer;
  I: Integer;
  AFieldMapData: TdxFieldMapData;
begin
  ACount := FieldsMapData.Count;
  for I := 0 to ACount - 1 do
  begin
    AFieldMapData := FieldsMapData[I];
    if AFieldMapData.MappedName <> '' then
      FMapFieldMappedName.Add(AFieldMapData.MappedName, AFieldMapData);
    if AFieldMapData.ColumnName <> '' then
      FMapColumnName.Add(AFieldMapData.ColumnName, AFieldMapData);
  end;

  DoFieldsMapDataChanged;
end;

procedure TdxDataSourceObjectProperties.SetColumnDelimiter(const Value: Char);
begin
  FColumnDelimiter := Value;
end;

procedure TdxDataSourceObjectProperties.SetDataSourceType(const Value: TdxMailMergeSourceType);
begin
  FDataSourceType := Value;
end;

procedure TdxDataSourceObjectProperties.SetFirstRowHeader(const Value: Boolean);
begin
  FFirstRowHeader := Value;
end;

procedure TdxDataSourceObjectProperties.SetTableName(const Value: string);
begin
  FTableName := Value;
end;

procedure TdxDataSourceObjectProperties.SetUdlConnectionString(const Value: string);
begin
  FUdlConnectionString := Value;
end;

procedure TdxDataSourceObjectProperties.SubscribeFieldsMapDataEvents;
begin
end;

procedure TdxDataSourceObjectProperties.UnsubscribeFieldsMapDataEvents;
begin
end;

{ TdxMailMergeProperties }

constructor TdxMailMergeProperties.Create;
begin
  inherited Create;
  FDataSourceObjectProperties := TdxDataSourceObjectProperties.Create;
end;

destructor TdxMailMergeProperties.Destroy;
begin
  FreeAndNil(FDataSourceObjectProperties);
  inherited Destroy;
end;

procedure TdxMailMergeProperties.DoConnectionStringChanged;
begin
  if not FConnectionStringChanged.Empty then
    FConnectionStringChanged.Invoke(Self, nil);
end;

procedure TdxMailMergeProperties.DoDataSourceChanged;
begin
  if not FDataSourceChanged.Empty then
    FDataSourceChanged.Invoke(Self, nil);
end;

procedure TdxMailMergeProperties.DoDataTypeChanged;
begin
  if not FDataTypeChanged.Empty then
    FDataTypeChanged.Invoke(Self, nil);
end;

procedure TdxMailMergeProperties.DoDestinationChanged;
begin
  if not FDestinationChanged.Empty then
    FDestinationChanged.Invoke(Self, nil);
end;

procedure TdxMailMergeProperties.DoEMailAddressColumnNameChanged;
begin
  if not FEMailAddressColumnNameChanged.Empty then
    FEMailAddressColumnNameChanged.Invoke(Self, nil);
end;

procedure TdxMailMergeProperties.DoLeaveBlankLinesChanged;
begin
  if not FLeaveBlankLinesChanged.Empty then
    FLeaveBlankLinesChanged.Invoke(Self, nil);
end;

procedure TdxMailMergeProperties.DoQueryChanged;
begin
  if not FQueryChanged.Empty then
    FQueryChanged.Invoke(Self, nil);
end;

procedure TdxMailMergeProperties.DoViewMergedDataChanged;
begin
  if not FViewMergedDataChanged.Empty then
    FViewMergedDataChanged.Invoke(Self, nil);
end;

procedure TdxMailMergeProperties.SetConnectionString(const Value: string);
begin
  if FConnectionString = Value then
    Exit;
  FConnectionString := Value;
  DoConnectionStringChanged;
end;

procedure TdxMailMergeProperties.SetDataSource(const Value: string);
begin
  if FDataSource = Value then
    Exit;
  FDataSource := Value;
  DoDataSourceChanged;
end;

procedure TdxMailMergeProperties.SetDataType(const Value: TdxMailMergeDataType);
begin
  if FDataType = Value then
    Exit;
  FDataType := Value;
  DoDataTypeChanged;
end;

procedure TdxMailMergeProperties.SetDestination(const Value: TdxMailMergeDestination);
begin
  if FDestination = Value then
    Exit;
  FDestination := Value;
  DoDestinationChanged;
end;

procedure TdxMailMergeProperties.SetEMailAddressColumnName(const Value: string);
begin
  if FEMailAddressColumnName = Value then
    Exit;
  FEMailAddressColumnName := Value;
  DoEMailAddressColumnNameChanged;
end;

procedure TdxMailMergeProperties.SetLeaveBlankLines(const Value: Boolean);
begin
  if FLeaveBlankLines = Value then
    Exit;
  FLeaveBlankLines := Value;
  DoLeaveBlankLinesChanged;
end;

procedure TdxMailMergeProperties.SetQuery(const Value: string);
begin
  if FQuery = Value then
    Exit;
  FQuery := Value;
  DoQueryChanged;
end;

procedure TdxMailMergeProperties.SetViewMergedData(const Value: Boolean);
begin
  if FViewMergedData = Value then
    Exit;
  FViewMergedData := Value;
  DoViewMergedDataChanged;
end;

{ TdxMailMergeOptions }

function TdxMailMergeOptions.GetDataSource: TDataSource;
begin
  Result := FDataSource;
end;

procedure TdxMailMergeOptions.SetDataSource(const Value: TDataSource);
begin
  FDataSource := Value;
end;

function TdxMailMergeOptions.GetCopyTemplateStyles: Boolean;
begin
  Result := FCopyTemplateStyles;
end;

function TdxMailMergeOptions.GetHeaderFooterLinkToPrevious: Boolean;
begin
  Result := FHeaderFooterLinkToPrevious;
end;

function TdxMailMergeOptions.GetMergeMode: TdxMergeMode;
begin
  Result := FMergeMode;
end;

function TdxMailMergeOptions.GetMergeRecords: TdxMergeRecords;
begin
  Result := FMergeRecords;
end;

procedure TdxMailMergeOptions.SetCopyTemplateStyles(const Value: Boolean);
begin
  FCopyTemplateStyles := Value;
end;

procedure TdxMailMergeOptions.SetHeaderFooterLinkToPrevious(const Value: Boolean);
begin
  FHeaderFooterLinkToPrevious := Value;
end;

procedure TdxMailMergeOptions.SetMergeMode(const Value: TdxMergeMode);
begin
  FMergeMode := Value;
end;

procedure TdxMailMergeOptions.SetMergeRecords(const Value: TdxMergeRecords);
begin
  FMergeRecords := Value;
end;

end.

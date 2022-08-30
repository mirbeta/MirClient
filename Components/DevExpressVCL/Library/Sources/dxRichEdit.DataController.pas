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

unit dxRichEdit.DataController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections, DB, Classes, Contnrs,
  dxCore, dxCoreClasses, cxCustomData, cxDBData,

  dxRichEdit.Types,
  dxRichEdit.DocumentModel.MailMerge,
  dxGenerics,
  dxRichEdit.Utils.Types;

type
  TdxOfficeDataController = class;

  TdxDataRow = Pointer;

  { TdxCurrentRowEventArgs }

  TdxCurrentRowEventArgs = class(TdxEventArgs);

  { TdxCurrentRowChangedEventArgs }

  TdxCurrentRowChangedEventArgs = class(TdxEventArgs)
  strict private
    FCurrentRow, FPreviousRow: TdxDataRow;
  public
    constructor Create(APreviousRow, ACurrentRow: TdxDataRow);

    property CurrentRow: TdxDataRow read FCurrentRow;
    property PreviousRow: TdxDataRow read FPreviousRow;
  end;

  { IdxDataControllerCurrentSupport }

  IdxDataControllerCurrentSupport = interface
    procedure CurrentControllerRowChanged(const E: TdxCurrentRowEventArgs);
    procedure CurrentControllerRowObjectChanged(const E: TdxCurrentRowChangedEventArgs);
  end;

  { TdxOfficeDataControllerAdapterBase }

  TdxOfficeDataControllerAdapterBase = class abstract(TcxIUnknownObject)
  strict private
    FOnCurrentRowChanged: TdxEventHandler;
    FOnDataSourceChanged: TdxEventHandler;
  protected
    function GetActiveRecord: Integer; virtual; abstract;
    function GetDataSource: TDataSource; virtual; abstract;
    function GetIsReady: Boolean; virtual; abstract;
    function GetMergeRecords: TdxMergeRecords; virtual; abstract;
    function GetRecordCount: Integer; virtual; abstract;
    procedure RaiseCurrentRowChangedEvent; virtual;
    procedure RaiseDataSourceChanged; virtual;
    procedure SetDataSource(const AValue: TDataSource); virtual; abstract;
    procedure SetMergeRecords(const Value: TdxMergeRecords); virtual; abstract;
  public
    function GetColumnIndex(const AName: string): Integer; virtual; abstract;
    function GetCurrentRowValue(AColumnIndex: Integer): Variant; virtual; abstract;

    function First: Boolean; virtual; abstract;
    function Next: Boolean; virtual; abstract;

    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property IsReady: Boolean read GetIsReady;
    property MergeRecords: TdxMergeRecords read GetMergeRecords write SetMergeRecords;

    property ActiveRecord: Integer read GetActiveRecord;
    property CurrentRowChanged: TdxEventHandler read FOnCurrentRowChanged;
    property DataSourceChanged: TdxEventHandler read FOnDataSourceChanged;
    property RecordCount: Integer read GetRecordCount;
  end;

  { TdxRichEditDataControllerAdapterBase }

  TdxRichEditDataControllerAdapterBase = class abstract(TdxOfficeDataControllerAdapterBase)
  public
    function GetColumnNames: TArray<TdxMergeFieldName>; virtual; abstract;
  end;

  { TdxRichEditDataControllerAdapter }

  TdxRichEditDataControllerAdapter = class(TdxRichEditDataControllerAdapterBase,
    IdxDataControllerCurrentSupport)
  strict private
    FDataController: TdxOfficeDataController;
    FComplexColumnNames: TdxStringBooleanDictionary;
    function GetCanUseFastProperties: Boolean;
    function GetHasUserFilter: Boolean;
  protected
    function GetActiveRecord: Integer; override;
    function GetDataController: TdxOfficeDataController; virtual;
    function GetDataSource: TDataSource; override;
    function GetIsReady: Boolean; override;
    function GetMergeRecords: TdxMergeRecords; override;
    function GetRecordCount: Integer; override;
    procedure SetDataSource(const AValue: TDataSource); override;
    procedure SetMergeRecords(const Value: TdxMergeRecords); override;

    procedure DisposeDataController; virtual;
    procedure InitializeDataController; virtual;
    procedure ListSourceChanged(ASender: TObject; E: TdxEventArgs); virtual;

    procedure SubscribeDataControllerEvents; virtual;
    procedure UnsubscribeDataControllerEvents; virtual;

    property DataController: TdxOfficeDataController read GetDataController;
  public
    constructor Create;
    destructor Destroy; override;

    function GetColumnIndex(const AName: string): Integer; override;
    function GetColumnNames: TArray<TdxMergeFieldName>; override;
    function GetCurrentRowValue(AColumnIndex: Integer): Variant; override;

    function First: Boolean; override;
    function Next: Boolean; override;

    function IsRowFit(AListSourceRow: Integer; AFit: Boolean): TdxNullableBoolean;
    procedure CurrentControllerRowChanged(const E: TdxCurrentRowEventArgs);
    procedure CurrentControllerRowObjectChanged(const E: TdxCurrentRowChangedEventArgs);

    property CanUseFastProperties: Boolean read GetCanUseFastProperties;
    property HasUserFilter: Boolean read GetHasUserFilter;
  end;

  { TdxOfficeDataControllerDataLink }

  TdxOfficeDataControllerDataLink = class(TDataLink)
  private
    FController: TdxOfficeDataController;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
  public
    constructor Create(AController: TdxOfficeDataController);
    destructor Destroy; override;
  end;

  { TdxOfficeDataController }

  TdxOfficeDataController = class
  strict private
    FDataLink: TdxOfficeDataControllerDataLink;
    FMergeRecords: TdxMergeRecords;
    FOnListSourceChanged: TdxEventHandler;
    function GetActiveRecord: Integer;
    function GetDataSource: TDataSource;
    function GetRecordCount: Integer;
    procedure SetDataSource(const Value: TDataSource);
  protected
    function GetIsReady: Boolean; virtual;
    procedure DoChanged; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function First: Boolean;
    function Next: Boolean;

    function GetFieldIndex(const AName: string): Integer;
    function GetCurrentRowValue(AFieldIndex: Integer): Variant;

    property IsReady: Boolean read GetIsReady;

    property ActiveRecord: Integer read GetActiveRecord;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property MergeRecords: TdxMergeRecords read FMergeRecords write FMergeRecords;
    property RecordCount: Integer read GetRecordCount;

    property ListSourceChanged: TdxEventHandler read FOnListSourceChanged;
  end;

implementation

uses
  Variants, cxVariants;

{ TdxCurrentRowChangedEventArgs }

constructor TdxCurrentRowChangedEventArgs.Create(APreviousRow, ACurrentRow: TdxDataRow);
begin
  inherited Create;
  FCurrentRow := ACurrentRow;
  FPreviousRow := APreviousRow;
end;

{ TdxOfficeDataControllerAdapterBase }

procedure TdxOfficeDataControllerAdapterBase.RaiseCurrentRowChangedEvent;
begin
  if not FOnCurrentRowChanged.Empty then
    FOnCurrentRowChanged.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxOfficeDataControllerAdapterBase.RaiseDataSourceChanged;
begin
  if not FOnDataSourceChanged.Empty then
    FOnDataSourceChanged.Invoke(Self, TdxEventArgs.Empty);
end;

{ TdxRichEditDataControllerAdapter }

constructor TdxRichEditDataControllerAdapter.Create;
begin
  inherited Create;
  FComplexColumnNames := TdxStringBooleanDictionary.Create;
  FDataController := TdxOfficeDataController.Create;
  InitializeDataController;
end;

destructor TdxRichEditDataControllerAdapter.Destroy;
begin
  FreeAndNil(FComplexColumnNames);
  FreeAndNil(FDataController);
  inherited Destroy;
end;

function TdxRichEditDataControllerAdapter.GetActiveRecord: Integer;
begin
  Result := DataController.ActiveRecord;
end;

function TdxRichEditDataControllerAdapter.GetDataController: TdxOfficeDataController;
begin
  Result := FDataController;
end;

function TdxRichEditDataControllerAdapter.GetIsReady: Boolean;
begin
  Result := DataController.IsReady;
end;

function TdxRichEditDataControllerAdapter.GetMergeRecords: TdxMergeRecords;
begin
  Result := DataController.MergeRecords;
end;

function TdxRichEditDataControllerAdapter.GetRecordCount: Integer;
begin
  Result := DataController.RecordCount;
end;

function TdxRichEditDataControllerAdapter.GetDataSource: TDataSource;
begin
  Result := DataController.DataSource;
end;

procedure TdxRichEditDataControllerAdapter.SetDataSource(const AValue: TDataSource);
begin
  DataController.DataSource := AValue;
end;

procedure TdxRichEditDataControllerAdapter.SetMergeRecords(const Value: TdxMergeRecords);
begin
  DataController.MergeRecords := Value;
end;

procedure TdxRichEditDataControllerAdapter.InitializeDataController;
begin
  SubscribeDataControllerEvents;
end;

procedure TdxRichEditDataControllerAdapter.DisposeDataController;
begin
  UnsubscribeDataControllerEvents;
  FreeAndNil(FDataController);
end;

function TdxRichEditDataControllerAdapter.GetColumnNames: TArray<TdxMergeFieldName>;
var
  AMergeFieldNames: TdxList<TdxMergeFieldName>;
  ADataSet: TDataSet;
  AField: TField;
begin
  if (DataController.DataSource = nil) or (DataController.DataSource.DataSet = nil) then
    Exit(nil);
  ADataSet := DataController.DataSource.DataSet;
  AMergeFieldNames := TdxList<TdxMergeFieldName>.Create;
  try
    for AField in ADataSet.Fields do
      AMergeFieldNames.Add(TdxMergeFieldName.Create(AField.FieldName, AField.DisplayLabel));
    for AField in ADataSet.AggFields do
      AMergeFieldNames.Add(TdxMergeFieldName.Create(AField.FieldName, AField.DisplayLabel));
    Result := AMergeFieldNames.ToArray;
  finally
    AMergeFieldNames.Free;
  end;
end;

function TdxRichEditDataControllerAdapter.GetColumnIndex(const AName: string): Integer;
begin
  Result := DataController.GetFieldIndex(AName);
  if Result < 0 then
  begin
    if FComplexColumnNames.ContainsKey(AName) then
      Exit(-1);
    FComplexColumnNames.Add(AName, True);
    Result := DataController.GetFieldIndex(AName);
  end;
end;

function TdxRichEditDataControllerAdapter.GetCurrentRowValue(AColumnIndex: Integer): Variant;
begin
  Result := DataController.GetCurrentRowValue(AColumnIndex);
end;

function TdxRichEditDataControllerAdapter.First: Boolean;
begin
  Result := DataController.First;
end;

function TdxRichEditDataControllerAdapter.Next: Boolean;
begin
  Result := DataController.Next;
end;

procedure TdxRichEditDataControllerAdapter.ListSourceChanged(ASender: TObject; E: TdxEventArgs);
begin
  RaiseDataSourceChanged;
end;

procedure TdxRichEditDataControllerAdapter.SubscribeDataControllerEvents;
begin
  DataController.ListSourceChanged.Add(ListSourceChanged);
end;

procedure TdxRichEditDataControllerAdapter.UnsubscribeDataControllerEvents;
begin
  DataController.ListSourceChanged.Remove(ListSourceChanged);
end;

procedure TdxRichEditDataControllerAdapter.CurrentControllerRowChanged(const E: TdxCurrentRowEventArgs);
begin
  RaiseCurrentRowChangedEvent;
end;

procedure TdxRichEditDataControllerAdapter.CurrentControllerRowObjectChanged(const E: TdxCurrentRowChangedEventArgs);
begin
  RaiseCurrentRowChangedEvent;
end;

function TdxRichEditDataControllerAdapter.GetCanUseFastProperties: Boolean;
begin
  Result := True;
end;

function TdxRichEditDataControllerAdapter.GetHasUserFilter: Boolean;
begin
  Result := False;
end;

function TdxRichEditDataControllerAdapter.IsRowFit(AListSourceRow: Integer; AFit: Boolean): TdxNullableBoolean;
begin
  Result := TdxNullableBoolean.Null;
end;

{ TdxOfficeDataControllerDataLink }

constructor TdxOfficeDataControllerDataLink.Create(AController: TdxOfficeDataController);
begin
  inherited Create;
  FController := AController;
  VisualControl := True;
end;

destructor TdxOfficeDataControllerDataLink.Destroy;
begin
  FController := nil;
  inherited Destroy;
end;

procedure TdxOfficeDataControllerDataLink.DataSetChanged;
begin
  FController.DoChanged;
end;

procedure TdxOfficeDataControllerDataLink.ActiveChanged;
begin
  FController.DoChanged;
end;

{ TdxOfficeDataController }

constructor TdxOfficeDataController.Create;
begin
  inherited Create;
  FDataLink := TdxOfficeDataControllerDataLink.Create(Self);
end;

destructor TdxOfficeDataController.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

function TdxOfficeDataController.First: Boolean;
begin
  if MergeRecords <> TdxMergeRecords.All then
    Exit(True);
  FDataLink.DataSet.First;
  Result := not FDataLink.DataSet.Eof;
end;

function TdxOfficeDataController.Next: Boolean;
begin
  if MergeRecords <> TdxMergeRecords.All then
    Exit(False);
  FDataLink.DataSet.Next;
  Result := not FDataLink.DataSet.Eof;
end;

function TdxOfficeDataController.GetFieldIndex(const AName: string): Integer;
var
  AField: TField;
begin
  if FDataLink.DataSet = nil then
    Exit(-1);
  AField := FDataLink.DataSet.FindField(AName);
  if AField = nil then
    Exit(-1);
  Result := AField.Index;
end;

function TdxOfficeDataController.GetCurrentRowValue(AFieldIndex: Integer): Variant;
begin
  if FDataLink.DataSet = nil then
    Exit(Null);
  if (AFieldIndex < 0) or (AFieldIndex >= FDataLink.DataSet.Fields.Count) then
    Exit(Null);

  Result := FDataLink.DataSet.Fields[AFieldIndex].Value;
  if VarIsDate(Result) then
    Result := VarToDateTime(Result);
end;

function TdxOfficeDataController.GetIsReady: Boolean;
begin
  Result := FDataLink.Active and (FDataLink.DataSet.FieldCount > 0);
end;

procedure TdxOfficeDataController.DoChanged;
begin
  if not ListSourceChanged.Empty then
    ListSourceChanged.Invoke(Self, TdxEventArgs.Empty);
end;

function TdxOfficeDataController.GetActiveRecord: Integer;
begin
  Result := -1;
  if not FDataLink.Active then
    Exit;
  Result := FDataLink.ActiveRecord;
end;

function TdxOfficeDataController.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TdxOfficeDataController.GetRecordCount: Integer;
begin
  Result := 0;
  if not FDataLink.Active then
    Exit;
  Result := FDataLink.RecordCount;
end;

procedure TdxOfficeDataController.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

end.

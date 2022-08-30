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

unit dxRichEdit.DocumentModel.FieldDataService;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, Rtti,

  dxRichEdit.Utils.Types,
  dxRichEdit.DataController,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.MailMerge;

type
  { IdxMailMergeDataService }

  IdxMailMergeDataService = interface(IdxFieldDataService)
  ['{0C81F9FF-E3E8-46FE-9007-74FFB90451B7}']
    function GetActiveRecord: Integer;
    function GetRecordCount: Integer;
    function MoveFirstRecord: Boolean;
    function MoveNextRecord: Boolean;
    function StartMailMerge(const AOptions: IdxMailMergeOptions): IdxMailMergeDataService;
    procedure EndMailMerge(const AFieldDataService: IdxMailMergeDataService);
  end;

  { IdxDynamicObjectPropertyValueProvider }

  IdxDynamicObjectPropertyValueProvider = interface
    function IsDynamicTypeInstance(AInstance: TObject): Boolean;
    function GetPropertyValue(AInstance: TObject; const APropertyName: string): TValue;
  end;

  { TdxMailMergeDataService }

  TdxMailMergeDataService = class(TInterfacedObject, IdxMailMergeDataService, IdxFieldDataService)
  strict private
    const
      FieldNameFormatString = '<<%s>>';
  strict private
    FDataController: TdxRichEditDataControllerAdapterBase;
    FDynamicObjectPropertyValueProvider: IdxDynamicObjectPropertyValueProvider;
  protected
    function GetActiveRecord: Integer;
    function GetBoundMode: Boolean; virtual;
    function GetDataController: TdxRichEditDataControllerAdapterBase; virtual;
    function GetRecordCount: Integer;
    function CreateNew(ADataController: TdxRichEditDataControllerAdapterBase; const AOptions: IdxMailMergeOptions): IdxMailMergeDataService; virtual;
    function CreateMailMergeDataController: TdxRichEditDataControllerAdapterBase; virtual;
    function GetColumnIndexByName(AMailMergeProperties: TdxMailMergeProperties; const AFieldName: string; AMapFieldName: Boolean): Integer; virtual;

    property DataController: TdxRichEditDataControllerAdapterBase read GetDataController;
  public
    constructor Create(ADataController: TdxRichEditDataControllerAdapterBase);

    function MoveFirstRecord: Boolean; virtual;
    function MoveNextRecord: Boolean; virtual;
    function StartMailMerge(const AOptions: IdxMailMergeOptions): IdxMailMergeDataService;
    procedure EndMailMerge(const AFieldDataService: IdxMailMergeDataService);
    function GetFieldValue(AMailMergeProperties: TdxMailMergeProperties; const AFieldName: string;
      AMapFieldName: Boolean; AOptions: TdxMailMergeDataMode; APieceTable: TdxCustomPieceTable; AField: TdxField): TValue; virtual;

    procedure DestroyDataController;

    property BoundMode: Boolean read GetBoundMode;
    property DynamicObjectPropertyValueProvider: IdxDynamicObjectPropertyValueProvider read FDynamicObjectPropertyValueProvider write FDynamicObjectPropertyValueProvider;
  end;

  { TdxSingleReferenceValueEnumerator }

  TdxSingleReferenceValueEnumerator = class(TInterfacedObject, IEnumerator<Double>, IEnumerator)
  strict private
    FValue: Double;
    FResetted: Boolean;
  protected
    function GetCurrent: TObject;
  public
    constructor Create(AValue: Double);

    function IEnumerator<Double>.GetCurrent = IEnumeratorGetCurrent;
    function IEnumeratorGetCurrent: Double;
    function MoveNext: Boolean;
    procedure Reset;
  end;

implementation

uses
  dxRichEdit.Utils.Exceptions;

{ TdxMailMergeDataService }

constructor TdxMailMergeDataService.Create(ADataController: TdxRichEditDataControllerAdapterBase);
begin
  inherited Create;
  Assert(ADataController <> nil);
  FDataController := ADataController;
end;

function TdxMailMergeDataService.GetActiveRecord: Integer;
begin
  Result := FDataController.ActiveRecord;
end;

function TdxMailMergeDataService.GetBoundMode: Boolean;
begin
  Result := FDataController.IsReady;
end;

function TdxMailMergeDataService.GetDataController: TdxRichEditDataControllerAdapterBase;
begin
  Result := FDataController;
end;

function TdxMailMergeDataService.GetRecordCount: Integer;
begin
  Result := DataController.RecordCount;
end;

function TdxMailMergeDataService.MoveFirstRecord: Boolean;
begin
  Result := FDataController.First;
end;

function TdxMailMergeDataService.MoveNextRecord: Boolean;
begin
  Result := FDataController.Next;
end;

function TdxMailMergeDataService.StartMailMerge(const AOptions: IdxMailMergeOptions): IdxMailMergeDataService;
var
  AMailMergeController: TdxRichEditDataControllerAdapterBase;
begin
  Result := nil;
  AMailMergeController := CreateMailMergeDataController;
  try
    AMailMergeController.MergeRecords := AOptions.MergeRecords;
    if AOptions.DataSource <> nil then
    begin
      AMailMergeController.DataSource := AOptions.DataSource;
      if not AMailMergeController.IsReady then
        Exit;
    end
    else
      AMailMergeController.DataSource := DataController.DataSource;
    Result := CreateNew(AMailMergeController, AOptions);
    if Result.MoveFirstRecord then
      Exit
    else
      Result := nil;
  finally
    if Result = nil then
      AMailMergeController.Free;
  end;
end;

function TdxMailMergeDataService.CreateNew(ADataController: TdxRichEditDataControllerAdapterBase; const AOptions: IdxMailMergeOptions): IdxMailMergeDataService;
begin
  Result := TdxMailMergeDataService.Create(ADataController);
end;

function TdxMailMergeDataService.CreateMailMergeDataController: TdxRichEditDataControllerAdapterBase;
begin
  Result := TdxRichEditDataControllerAdapter.Create;
end;

procedure TdxMailMergeDataService.EndMailMerge(const AFieldDataService: IdxMailMergeDataService);
begin
  TdxMailMergeDataService(AFieldDataService).DestroyDataController;
end;

function TdxMailMergeDataService.GetFieldValue(AMailMergeProperties: TdxMailMergeProperties;
  const AFieldName: string; AMapFieldName: Boolean; AOptions: TdxMailMergeDataMode;
  APieceTable: TdxCustomPieceTable; AField: TdxField): TValue;
var
  AColumnIndex: Integer;
begin
  if not BoundMode then
    TdxRichEditExceptions.ThrowInternalException;
  if AOptions = TdxMailMergeDataMode.None then
    Exit(Format(FieldNameFormatString, [AFieldName]));
  AColumnIndex := GetColumnIndexByName(AMailMergeProperties, AFieldName, AMapFieldName);
  if AColumnIndex < 0 then
  begin
    AColumnIndex := DataController.GetColumnIndex(AFieldName);
  end;
  Result := TValue.FromVariant(DataController.GetCurrentRowValue(AColumnIndex));
end;

procedure TdxMailMergeDataService.DestroyDataController;
begin
  FreeAndNil(FDataController);
end;


function TdxMailMergeDataService.GetColumnIndexByName(AMailMergeProperties: TdxMailMergeProperties; const AFieldName: string; AMapFieldName: Boolean): Integer;
var
  ADataSourceObjectProperties: TdxDataSourceObjectProperties;
  AFieldMapData: TdxFieldMapData;
begin
  ADataSourceObjectProperties := AMailMergeProperties.DataSourceObjectProperties;
  if AMapFieldName then
    Exit(ADataSourceObjectProperties.FindMapDataByMapName(AFieldName).ColumnIndex)
  else
  begin
    AFieldMapData := ADataSourceObjectProperties.FindMapDataByColumnName(AFieldName);
    if AFieldMapData = nil then
      Exit(-1)
    else
      Exit(AFieldMapData.ColumnIndex);
  end;
end;

{ TdxSingleReferenceValueEnumerator }

constructor TdxSingleReferenceValueEnumerator.Create(AValue: Double);
begin
  inherited Create;
  FValue := AValue;
  Reset;
end;

function TdxSingleReferenceValueEnumerator.IEnumeratorGetCurrent: Double;
begin
  Result := FValue;
end;

function TdxSingleReferenceValueEnumerator.GetCurrent: TObject;
begin
  Result := nil;
end;

function TdxSingleReferenceValueEnumerator.MoveNext: Boolean;
begin
  Result := FResetted;
  if FResetted then
    FResetted := False;
end;

procedure TdxSingleReferenceValueEnumerator.Reset;
begin
  FResetted := True;
end;

end.

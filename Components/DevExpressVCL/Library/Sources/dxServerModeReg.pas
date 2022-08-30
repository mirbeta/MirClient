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

unit dxServerModeReg;

{$I cxVer.inc}

interface

uses
  SysUtils, Windows, Types, Classes, DesignEditors, DesignMenus, VCLEditors,
  TypInfo, TreeIntf, dxCoreReg, DB, dxServerModeData, dxServerModeSQLAdapters;

type
  { TdxServerModeCustomDataSourceEditor }

  TdxServerModeCustomDataSourceEditor = class(TdxComponentEditor)
  private
    function GetDataSource: TdxServerModeCustomDataSource;
  protected
    function GetProductName: string; override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;

    procedure ExecuteFieldsEditor;
  public
    property DataSource: TdxServerModeCustomDataSource read GetDataSource;
  end;

  { TdxServerModeCustomDataSourceSprig }

  TdxServerModeCustomDataSourceSprig = class(TComponentSprig)
  protected
    procedure ConnectionFigureParent(AConnection: TCustomConnection);
    function GetImpliedConnectionString: string; virtual;
  public
    procedure FigureParent; override;
    function AnyProblems: Boolean; override;
    function Caption: string; override;
  end;

  TdxServerModeDataSourceSprig = class(TdxServerModeCustomDataSourceSprig)
  public
    function AnyProblems: Boolean; override;
    function Caption: string; override;
  end;

  TdxServerModeQueryDataSourceSprig = class(TdxServerModeCustomDataSourceSprig)
  public
    function AnyProblems: Boolean; override;
  end;

procedure Register;

implementation

uses
  Graphics, Forms, Controls, StdCtrls, DsnDBCst, DsnDB, DesignIntf, cxPropEditors,
  dxServerModeDataSourceEditor;

type
  TdxServerModeCustomDataSourceAccess = class(TdxServerModeCustomDataSource);
  TdxServerModeDataControllerAccess = class(TdxServerModeDataController);
  TdxServerModeDataFieldAccess = class(TdxServerModeDataField);

  { TdxFieldSprig }

  TdxServerModeFieldSprig = class(TFieldSprig)
  public
    procedure FigureParent; override;
  end;

  { TdxServerModeTableNameProperty }

  TdxServerModeTableNameProperty = class(TStringProperty)
  private
    function GetDataSource: TdxServerModeDataSource;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxServerModeSummaryItemFieldNameProperty }

  TdxServerModeSummaryItemFieldNameProperty = class(TStringProperty)
  private
    function GetSummaryItem: TdxServerModeSummaryItem;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxServerModeKeyFieldNamesProperty }

  TdxServerModeKeyFieldNamesProperty = class(TStringProperty, ICustomPropertyListDrawing)
  private
    FKeyFieldNames: TStringList;
    function GetDataSource: TdxServerModeCustomDataSourceAccess;
  protected
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxServerModeSQLAdapterProperty }

  TdxServerModeSQLAdapterProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxServerModeSQLAdapterEventsProperty }

  TdxServerModeSQLAdapterEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

{ TdxServerModeFieldSprig }

procedure TdxServerModeFieldSprig.FigureParent;
var
  ADataSet: TDataSet;
begin
  ADataSet := TField(Item).DataSet;
  if (ADataSet <> nil) and (ADataSet.Owner is TdxServerModeCustomDataSource) then
    SeekParent(ADataSet.Owner).Add(Self)
  else
    inherited;
end;

{ TdxServerModeTableNameProperty }

function TdxServerModeTableNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TdxServerModeTableNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  AValues: TStringList;
begin
  AValues := TStringList.Create;
  try
    GetDataSource.PopulateTableNames(AValues);
    for I := 0 to AValues.Count - 1 do
      Proc(AValues[I]);
  finally
    AValues.Free;
  end;
end;

function TdxServerModeTableNameProperty.GetDataSource: TdxServerModeDataSource;
begin
  Result := GetComponent(0) as TdxServerModeDataSource;
end;

{ TdxServerModeSummaryItemFieldNameProperty }

function TdxServerModeSummaryItemFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TdxServerModeSummaryItemFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  ADataController: TdxServerModeDataControllerAccess;
  AFields: TFields;
  I: Integer;
begin
  ADataController := TdxServerModeDataControllerAccess(GetSummaryItem.DataController);
  if (ADataController <> nil) and (ADataController.DataSource <> nil) then
  begin
    AFields := ADataController.DataSource.Fields;
    for I := 0 to AFields.Count - 1 do
      Proc(AFields[I].FieldName);
  end;
end;

function TdxServerModeSummaryItemFieldNameProperty.GetSummaryItem: TdxServerModeSummaryItem;
begin
  Result := GetComponent(0) as TdxServerModeSummaryItem;
end;

{ TdxServerModeKeyFieldNamesProperty }

constructor TdxServerModeKeyFieldNamesProperty.Create(
  const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  FKeyFieldNames := TStringList.Create;
  FKeyFieldNames.CaseSensitive := False;
  FKeyFieldNames.Sorted := True;
end;

destructor TdxServerModeKeyFieldNamesProperty.Destroy;
begin
  FreeAndNil(FKeyFieldNames);
  inherited Destroy;
end;

function TdxServerModeKeyFieldNamesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TdxServerModeKeyFieldNamesProperty.GetDataSource: TdxServerModeCustomDataSourceAccess;
begin
  Result := TdxServerModeCustomDataSourceAccess(GetComponent(0));
end;

procedure TdxServerModeKeyFieldNamesProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  FKeyFieldNames.Clear;
  GetDataSource.InitializeDataSet;
  GetDataSource.DoPopulateKeyFields(FKeyFieldNames);
  for I := 0 to GetDataSource.FieldDefs.Count - 1 do
    Proc(GetDataSource.FieldDefs[I].Name);
end;

procedure TdxServerModeKeyFieldNamesProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  AIsKeyField: Boolean;
begin
  AIsKeyField := FKeyFieldNames.IndexOf(Value) >= 0;
  if AIsKeyField then
    ACanvas.Font.Style := [fsBold]
  else
    ACanvas.Font.Style := [];
  if not ASelected then
  begin
    ACanvas.Brush.Color := clWindow;
    ACanvas.FillRect(ARect);
    if AIsKeyField then
      ACanvas.Font.Color := clWindowText
    else
      ACanvas.Font.Color := clGrayText;
  end
  else
  begin
    ACanvas.Brush.Color := clHighlight;
    ACanvas.FillRect(ARect);
    ACanvas.Font.Color := clHighlightText;
  end;
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextOut(ARect.Left + 2, ARect.Top + 2, Value);
  ACanvas.Brush.Style := bsSolid;
end;

procedure TdxServerModeKeyFieldNamesProperty.ListMeasureHeight(
  const Value: string; ACanvas: TCanvas; var AHeight: Integer);
begin
end;

procedure TdxServerModeKeyFieldNamesProperty.ListMeasureWidth(
  const Value: string; ACanvas: TCanvas; var AWidth: Integer);
begin
end;

{ TdxServerModeSQLAdapterProperty }

function TdxServerModeSQLAdapterProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties)
  else
    Include(Result, paSubProperties);
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TdxServerModeSQLAdapterProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := dxGetServerModeSQLAdapters.GetDescriptionByClass(
      TdxServerModeCustomSQLAdapter(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TdxServerModeSQLAdapterProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  ADesc: string;
begin
  for I := 0 to dxGetServerModeSQLAdapters.Count - 1 do
  begin
    ADesc := dxGetServerModeSQLAdapters.Descriptions[I];
    if ADesc <> '' then
      Proc(ADesc);
  end;
end;

function TdxServerModeSQLAdapterProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := (GetComponent(I) is TdxServerModeCustomDataSource) and
      (TdxServerModeCustomDataSource(GetComponent(I)).SQLAdapter <> nil);
    if not Result then Exit;
  end;
  Result := True;
end;

procedure TdxServerModeSQLAdapterProperty.SetValue(const Value: string);
var
  AdapterClass: TdxServerModeCustomSQLAdapterClass;
  I: Integer;
begin
  AdapterClass := TdxServerModeCustomSQLAdapterClass(dxGetServerModeSQLAdapters.FindByClassName(Value));
  if AdapterClass = nil then
    AdapterClass := TdxServerModeCustomSQLAdapterClass(dxGetServerModeSQLAdapters.FindByDescription(Value));
  for I := 0 to PropCount - 1 do
    TdxServerModeCustomDataSource(GetComponent(I)).SQLAdapterClass := AdapterClass;
  Modified;
end;

{ TdxServerModeSQLAdapterEventsProperty }

function TdxServerModeSQLAdapterEventsProperty.GetInstance: TPersistent;
begin
  Result := TdxServerModeCustomDataSource(GetComponent(0)).SQLAdapter;
end;

{ TdxServerModeCustomDataSourceEditor }

function TdxServerModeCustomDataSourceEditor.GetProductName: string;
begin
  Result := 'ExpressDataController';
end;

function TdxServerModeCustomDataSourceEditor.InternalGetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := 'Fields Editor...'
  else
    Result := inherited InternalGetVerb(AIndex);
end;

function TdxServerModeCustomDataSourceEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxServerModeCustomDataSourceEditor.InternalExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    ExecuteFieldsEditor
  else
    inherited InternalExecuteVerb(AIndex);
end;

procedure TdxServerModeCustomDataSourceEditor.ExecuteFieldsEditor;
begin
  dxShowServerModeDataSourceEditor(Designer, DataSource);
end;

function TdxServerModeCustomDataSourceEditor.GetDataSource: TdxServerModeCustomDataSource;
begin
  Result := TdxServerModeCustomDataSource(Component);
end;

{ TdxServerModeCustomDataSourceSprig }

function TdxServerModeCustomDataSourceSprig.AnyProblems: Boolean;
begin
  Result := True;
end;

function TdxServerModeCustomDataSourceSprig.Caption: string;
begin
  Result := CaptionFor(TdxServerModeCustomDataSource(Item).Name, UniqueName);
end;

procedure TdxServerModeCustomDataSourceSprig.ConnectionFigureParent(
  AConnection: TCustomConnection);
var
  AConnectionSprig: TSprig;
begin
  if AConnection <> nil then
    AConnectionSprig := Root.Find(AConnection, False)
  else
    AConnectionSprig := nil;
  if (AConnectionSprig = nil) and (GetImpliedConnectionString <> '') then
    AConnectionSprig := Root.Find(GetImpliedConnectionString, False);
  if AConnectionSprig <> nil then
    AConnectionSprig.Add(Self)
  else
    inherited FigureParent;
end;

procedure TdxServerModeCustomDataSourceSprig.FigureParent;
begin
  with TdxServerModeCustomDataSource(Item) do
    ConnectionFigureParent(Connection);
end;

function TdxServerModeCustomDataSourceSprig.GetImpliedConnectionString: string;
begin
  Result := '';
end;

{ TdxServerModeDataSourceSprig }

function TdxServerModeDataSourceSprig.AnyProblems: Boolean;
begin
  Result := TdxServerModeDataSource(Item).TableName = '';
end;

function TdxServerModeDataSourceSprig.Caption: string;
begin
  Result := CaptionFor(TdxServerModeDataSource(Item).TableName, UniqueName);
end;

{ TdxServerModeQueryDataSourceSprig }

function TdxServerModeQueryDataSourceSprig.AnyProblems: Boolean;
begin
  Result := TdxServerModeQueryDataSource(Item).SQL.Text = '';
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
{$IFNDEF NONDB}
  RegisterClasses([TdxServerModeCustomDataSource]);
  RegisterPropertyEditor(TypeInfo(string), TdxServerModeDataSource, 'TableName', TdxServerModeTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxServerModeCustomDataSource, 'KeyFieldNames', TdxServerModeKeyFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(TdxServerModeCustomSQLAdapter), TdxServerModeCustomDataSource, 'SQLAdapter', TdxServerModeSQLAdapterProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxServerModeCustomDataSource, 'SQLAdapterClassName', nil);
  RegisterPropertyEditor(TypeInfo(string), TdxServerModeSummaryItem, 'FieldName', TdxServerModeSummaryItemFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TdxServerModeCustomDataSource, 'SQLAdapterEvents', TdxServerModeSQLAdapterEventsProperty);
  RegisterComponentEditor(TdxServerModeCustomDataSource, TdxServerModeCustomDataSourceEditor);
  RegisterSprigType(TdxServerModeDataSource, TdxServerModeDataSourceSprig);
  RegisterSprigType(TdxServerModeQueryDataSource, TdxServerModeQueryDataSourceSprig);
  RegisterSprigType(TField, TdxServerModeFieldSprig);
{$ENDIF}
end;

end.

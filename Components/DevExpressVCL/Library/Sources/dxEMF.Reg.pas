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

unit dxEMF.Reg;

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  VCLEditors, DesignIntf, DesignEditors, DesignMenus, ColnEdit,
  Rtti, TypInfo, IOUtils,
  DB, DBReg, DSDesign, DsnDBCst,
  Forms, Controls, StdCtrls,
  Dialogs,
  dxCore, dxCoreReg,
  dxEMF.Core,
  dxEMF.Data,
  dxEMF.DataSet,
  dxEMF.Types;

type

  { TdxEMFPackageNameProperty }

  TdxEMFPackageNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxEMFCriteriaOperatorTextProperty }

  TdxEMFCriteriaOperatorTextProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TdxEMFSelectExpressionTextProperty }

  TdxEMFSelectExpressionTextProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  dxEMF.DB.SQLConnectionProvider,
  dxEMF.DB.MSAccess,
  dxEMF.DB.MSSQL,
  dxEMF.DB.MySQL,
  dxEMF.DB.SQLite,
  dxEMF.DB.Oracle,
  dxEMF.DB.Firebird,
  dxEMF.DB.Criteria,
  dxEMF.Core.Reg,
  dxEMF.Metadata,
  dxEMF.DB.Model,
  dxEMF.Utils,
  dxEMF.Linq,
  dxEMF.Design.IndexFieldsEdit;

type
  TdxEMFDataProviderOptionsAccess = class(TdxEMFDataProviderOptions);
  TdxEMFCustomDataContextAccess = class(TdxEMFCustomDataContext);
  TdxEMFCustomDataSetAccess = class(TdxEMFCustomDataSet);
  TdxEMFCustomQueryDataSetAccess = class(TdxEMFCustomQueryDataSet);
  TdxCustomEMFQueryDataSourceAccess = class(TdxCustomEMFQueryDataSource);
  TdxEMFTableAccess = class(TdxEMFTable);
  TdxEntityInfoAccess = class(TdxEntityInfo);

  { TdxEMFSessionComponentEditor }

  TdxEMFSessionComponentEditor = class(TdxEMFCustomComponentEditor)
  protected
    procedure DataModeling;
  end;

  { TdxEMFCustomConnectionSelectionEditor }

  TdxEMFCustomConnectionSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxEMFDBEngineProperty }

  TdxEMFDBEngineProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


  { TdxEMFLinqDataContextNameProperty }

  TdxEMFLinqDataContextNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxEMFPackageFileNameProperty }

  TdxEMFPackageFileNameProperty  = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TdxEMFEntityNameProperty }

  TdxEMFEntityNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxEMFIndexNameProperty }

  TdxEMFIndexNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxCustomEMFFieldNamesProperty }

  TdxCustomEMFFieldNamesProperty = class(TStringProperty)
  protected
    procedure DoEdit(ADataSet: TdxEMFCustomDataSet);
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxEMFFieldNamesProperty }

  TdxEMFFieldNamesProperty = class(TdxCustomEMFFieldNamesProperty)
  public
    procedure Edit; override;
  end;

  { TdxEMFMasterFieldNamesProperty }

  TdxEMFMasterFieldNamesProperty = class(TdxCustomEMFFieldNamesProperty)
  public
    procedure Edit; override;
  end;

  { TdxEMFDataSetEditor }

  TdxEMFDataSetEditor = class(TdxEMFCustomComponentEditor)
  protected
    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
  end;

  { TdxEMFTableEditor }

  TdxEMFTableEditor = class(TdxEMFCustomComponentEditor)
  protected
    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
  end;

{ TdxEMFDSDesigner }

  TdxEMFDSDesigner = class(TDSDesigner)
  public
    function SupportsInternalCalc: Boolean; override;
  end;

{ TdxEMFTableDesigner }

  TdxEMFTableDesigner = class(TdxEMFDSDesigner)
  public
    procedure BeginUpdateFieldDefs; override;
  end;

  { TContextDesignerController }

  TContextDesignerController = class
  private
    FContext: TRttiContext;
    FModules: TList<HMODULE>;
  protected
    function IsEntitiesPackages(ARttiPackage: TRttiPackage): Boolean;
    procedure UnloadPackages;
    property Context: TRttiContext read FContext;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadPackage(const AFileName: string): TRttiPackage;
    function GetInstalledPackages: TArray<TRttiPackage>;
  end;

  { TdxLinqExpressionFactoryHelper }

  TdxLinqExpressionFactoryHelper = class helper for TdxLinqExpressionFactory
  public
    class function IsRegistered(AEntity: TClass): Boolean; overload;
    class function GetEntities: TArray<TClass>;
  end;

const
  sdxDataModeling = 'Data modeling...';

{ TdxLinqExpressionFactoryHelper }

class function TdxLinqExpressionFactoryHelper.GetEntities: TArray<TClass>;
begin
  if Expressions = nil then
    Exit(nil);
  Result := Expressions.Keys.ToArray;
end;

class function TdxLinqExpressionFactoryHelper.IsRegistered(AEntity: TClass): Boolean;
begin
  Result := (Expressions <> nil) and Expressions.ContainsKey(AEntity);
end;

{ TContextDesignerController }

constructor TContextDesignerController.Create;
begin
  inherited Create;
  FContext := TRttiContext.Create;
  FModules := TList<HMODULE>.Create;
end;

destructor TContextDesignerController.Destroy;
begin
  UnloadPackages;
  FreeAndNil(FModules);
  inherited;
end;

function TContextDesignerController.GetInstalledPackages: TArray<TRttiPackage>;
var
  APackages: TArray<TRttiPackage>;
  ARttiPackage: TRttiPackage;
  AResult: TList<TRttiPackage>;
begin
  AResult := TList<TRttiPackage>.Create;
  try
    APackages := FContext.GetPackages;
    for ARttiPackage in APackages do
      if IsEntitiesPackages(ARttiPackage) then
        AResult.Add(ARttiPackage);
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

function TContextDesignerController.IsEntitiesPackages(ARttiPackage: TRttiPackage): Boolean;
var
  ATypes: TArray<TRttiType>;
  AType: TRttiType;
  AClass: TClass;
begin
  ATypes := ARttiPackage.GetTypes;
  for AType in ATypes do
    if AType.IsInstance then
    begin
      AClass := GetTypeData(AType.Handle).ClassType;
      if TdxLinqExpressionFactory.IsRegistered(AClass) then
        Exit(True);
    end;
  Result := False;
end;

function TContextDesignerController.LoadPackage(const AFileName: string): TRttiPackage;
var
  AModule: HMODULE;
  APackages: TArray<TRttiPackage>;
begin
  AModule := SysUtils.LoadPackage(AFileName);
  APackages := FContext.GetPackages;
  for Result in APackages do
    if SameText(Result.Name, AFileName) and IsEntitiesPackages(Result) then
    begin
      FModules.Add(AModule);
      Exit;
    end;
  SysUtils.UnLoadPackage(AModule);
  Result := nil;
end;

procedure TContextDesignerController.UnloadPackages;
var
  AModule: HMODULE;
begin
  for AModule in FModules do
    try
      UnloadPackage(AModule);
    except
    end;
end;

{ TdxEMFTableDesigner }

procedure TdxEMFTableDesigner.BeginUpdateFieldDefs;
var
  ATable: TdxEMFTableAccess;
begin
  if not DataSet.Active then
  begin
    ATable := TdxEMFTableAccess(DataSet as TdxEMFTable);
    DataSet.FieldDefs.Updated := False;
    ATable.DoAssignEntity;
  end;
end;

{ TdxEMFDSDesigner }

function TdxEMFDSDesigner.SupportsInternalCalc: Boolean;
begin
  Result := True;
end;

{ TdxEMFSessionComponentEditor }

procedure TdxEMFSessionComponentEditor.DataModeling;
begin
end;

{ TdxEMFCustomConnectionSelectionEditor }

procedure TdxEMFCustomConnectionSelectionEditor.RequiresUnits(Proc: TGetStrProc);
var
  I: Integer;
  AComponent: TComponent;
  AItem: TdxEMFCustomDataProvider;
  AProviderSQLClass: TdxSQLConnectionProviderClass;
begin
  for I := 0 to Designer.Root.ComponentCount - 1 do
  begin
    AComponent := Designer.Root.Components[I];
    if AComponent is TdxEMFCustomDataProvider then
    begin
      AItem := TdxEMFCustomDataProvider(AComponent);
      AProviderSQLClass := TdxEMFDataProviderOptionsAccess(AItem.Options).ProviderSQLClass;
      if AProviderSQLClass <> nil then
        Proc(AProviderSQLClass.UnitName);
    end;
  end;
end;

{ TdxEMFDBEngineProperty }

function TdxEMFDBEngineProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TdxEMFDBEngineProperty.GetValues(Proc: TGetStrProc);
var
  AValues: TList<TdxDBEngine>;
  I: Integer;
begin
  AValues := TList<TdxDBEngine>.Create;
  try
    TdxSQLConnectionProviderFactory.PopulateDBEngines(AValues);
    for I := 0 to AValues.Count - 1 do
      Proc(AValues[I]);
  finally
    AValues.Free;
  end;
end;


{ TdxEMFDataSetEditor }

procedure TdxEMFDataSetEditor.InternalExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    ShowFieldsEditor(Designer, TdxEMFDataSet(Component), TdxEMFDSDesigner);
end;

function TdxEMFDataSetEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := SDatasetDesigner;
end;

function TdxEMFDataSetEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

{ TdxEMFTableEditor }

procedure TdxEMFTableEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: ShowFieldsEditor(Designer, TdxEMFDataSet(Component), TdxEMFDSDesigner);
  end;
end;

function TdxEMFTableEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := SDatasetDesigner;
end;

function TdxEMFTableEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

{ TdxEMFLinqDataContextNameProperty }

function TdxEMFLinqDataContextNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TdxEMFLinqDataContextNameProperty.GetValues(Proc: TGetStrProc);
var
  ADataContexts: TArray<PTypeInfo>;
  ATypeInfo: PTypeInfo;
begin
  ADataContexts := TdxEMFCustomDataContextAccess.GetDataContexts;
  for ATypeInfo in ADataContexts do
    Proc(GetTypeName(ATypeInfo));
end;


{ TdxEMFPackageFileNameProperty }

procedure TdxEMFPackageFileNameProperty.Edit;
var
  AOpenDialog: TOpenDialog;
begin
  AOpenDialog := TOpenDialog.Create(nil);
  try
    AOpenDialog.Options := AOpenDialog.Options + [ofFileMustExist];
    AOpenDialog.Filter := 'All packages|*.bpl';
    if AOpenDialog.Execute then
      SetValue(AOpenDialog.FileName);
  finally
    AOpenDialog.Free;
  end;
end;

function TdxEMFPackageFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paRevertable, paDialog, paMultiSelect];
end;

{ TdxEMFPackageNameProperty }

function TdxEMFPackageNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TdxEMFPackageNameProperty.GetValues(Proc: TGetStrProc);
var
  AController: TContextDesignerController;
  AInstalledPackages: TArray<TRttiPackage>;
  APackage: TRttiPackage;
begin
  AController := TContextDesignerController.Create;
  try
    AInstalledPackages := AController.GetInstalledPackages;
    for APackage in AInstalledPackages do
      Proc(TPath.GetFileNameWithoutExtension(APackage.Name));
  finally
    AController.Free;
  end;
end;

{ TdxEMFEntityNameProperty }

function TdxEMFEntityNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TdxEMFEntityNameProperty.GetValues(Proc: TGetStrProc);
var
  ADataSet: TdxEMFCustomDataSet;
  AEntityClasses: TArray<TClass>;
  AClass: TClass;
begin
  ADataSet := GetComponent(0) as TdxEMFCustomDataSet;
  if TdxEMFCustomDataSetAccess(ADataSet).DataContext <> nil then
    AEntityClasses := TdxEMFCustomDataContextAccess(TdxEMFCustomDataSetAccess(ADataSet).DataContext).GetEntityClasses()
  else
    if TdxEMFCustomDataSetAccess(ADataSet).PackageName <> '' then
      AEntityClasses := TdxEMFCustomDataContextAccess.GetEntityClasses(TdxEMFCustomDataSetAccess(ADataSet).PackageName)
    else
      Exit;
  for AClass in AEntityClasses do
    Proc(TdxEntityInfoAccess.GetQualifiedClassName(AClass));
end;

{ TdxEMFIndexNameProperty }

function TdxEMFIndexNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TdxEMFIndexNameProperty.GetValues(Proc: TGetStrProc);
var
  ADataSet: TdxEMFTable;
  AEntityInfo: TdxEntityInfo;
  ADBIndex: TdxDBIndex;
begin
  ADataSet := GetComponent(0) as TdxEMFTable;
  if ADataSet.EntityName = '' then
    Exit;
  AEntityInfo := TdxEMFTableAccess(ADataSet).EntityInfo;
  if AEntityInfo = nil then
    Exit;
  for ADBIndex in AEntityInfo.DBTable.Indexes do
    Proc(TdxFormatterHelper.CommaText(ADBIndex.Columns, ';'));
end;

{ TdxCustomEMFFieldNamesProperty }

procedure TdxCustomEMFFieldNamesProperty.DoEdit(ADataSet: TdxEMFCustomDataSet);
var
  AEntityInfo: TdxEntityInfo;
  AMemberInfo: TdxMappingMemberInfo;
begin
  if (TdxEMFCustomDataSetAccess(ADataSet).EntityName = '') and (TdxEMFCustomDataSetAccess(ADataSet).EntityInfo = nil) then
    Exit;
  if TdxEMFCustomDataSetAccess(ADataSet).EntityInfo <> nil then
    AEntityInfo := TdxEMFCustomDataSetAccess(ADataSet).EntityInfo
  else
    AEntityInfo := TdxEMFCustomDataSetAccess(ADataSet).EntityInfo;
  if AEntityInfo = nil then
    Exit;
  with TIndexFieldsEditDlg.Create(Application) do
  try
    SourceFields.BeginUpdate;
    try
      for AMemberInfo in AEntityInfo.PersistentProperties do
        SourceFields.Add(AMemberInfo.ActualName);
    finally
      SourceFields.EndUpdate;
    end;
    IndexFieldNames := GetValue;
    if ShowModal = mrOk then
    begin
      SetValue(IndexFieldNames);
      Modified;
    end;
  finally
    Free;
  end;
end;

function TdxCustomEMFFieldNamesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

{ TdxEMFFieldNamesProperty }

procedure TdxEMFFieldNamesProperty.Edit;
begin
  DoEdit(GetComponent(0) as TdxEMFCustomDataSet);
end;

{ TdxEMFMasterFieldNamesProperty }

procedure TdxEMFMasterFieldNamesProperty.Edit;
var
  ATable: TdxEMFTable;
begin
  ATable := GetComponent(0) as TdxEMFTable;
  if (ATable.MasterSource <> nil) and (ATable.MasterSource.DataSet <> nil) then
    DoEdit(ATable.MasterSource.DataSet as TdxEMFCustomDataSet);
end;

{ TdxEMFSelectExpressionTextProperty }

function TdxEMFSelectExpressionTextProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TdxEMFSelectExpressionTextProperty.GetValues(Proc: TGetStrProc);
var
  AEMFDataSource: TdxCustomEMFQueryDataSource;
  AFieldExpression: TdxFieldExpression;
  I: Integer;
begin
  AEMFDataSource := ((GetComponent(0) as TCollectionItem).Collection as TdxExpressionDefinitions).EMFDataSource;
  for I := 0 to TdxCustomEMFQueryDataSourceAccess(AEMFDataSource).QueryProperties.FieldExpressions.Count - 1 do
  begin
    AFieldExpression := TdxCustomEMFQueryDataSourceAccess(AEMFDataSource).QueryProperties.FieldExpressions[I];
    Proc(AFieldExpression.ExpressionText);
  end;
end;

{ TdxEMFCriteriaOperatorTextProperty }

procedure TdxEMFCriteriaOperatorTextProperty.Edit;
begin

end;

function TdxEMFCriteriaOperatorTextProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

procedure Register;
begin
  RegisterEMFComponent(TdxEMFSession, TdxEMFSessionComponentEditor);
  RegisterEMFComponent(TdxEMFDataContext);
  RegisterEMFComponent(TdxEMFDataSet, TdxEMFDataSetEditor);
  RegisterEMFComponent(TdxEMFTable, TdxEMFTableEditor);
  RegisterFields([TdxEntityField]);
  RegisterPropertyEditor(TypeInfo(TdxDBEngine), TdxEMFDataProviderOptions, 'DBEngine', TdxEMFDBEngineProperty);

  RegisterPropertyEditor(TypeInfo(string), TdxEMFDataContext, 'PackageName', TdxEMFPackageNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxEMFDataContext, 'PackageFileName', TdxEMFPackageFileNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TdxEMFCustomDataSet, 'PackageName', TdxEMFPackageNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxEMFCustomDataSet, 'EntityName', TdxEMFEntityNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxEMFTable, 'DetailFields', TdxEMFFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxEMFTable, 'IndexName', TdxEMFIndexNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxEMFTable, 'IndexFieldNames', TdxEMFFieldNamesProperty);

  RegisterPropertyEditor(TypeInfo(string), nil, 'CriteriaText', TdxEMFCriteriaOperatorTextProperty);
  RegisterPropertyEditor(TypeInfo(string), nil, 'GroupCriteriaText', TdxEMFCriteriaOperatorTextProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxFieldExpression, 'ExpressionText', TdxEMFCriteriaOperatorTextProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxSortByExpressionDefinition, 'ExpressionText', TdxEMFSelectExpressionTextProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxGroupByExpressionDefinition, 'ExpressionText', TdxEMFSelectExpressionTextProperty);

  RegisterSelectionEditor(TdxEMFCustomDataProvider, TdxEMFCustomConnectionSelectionEditor);

  RegisterPropertiesInCategory(sDatabaseCategoryName, TdxEMFCustomDataSet,
    ['EntityName', 'DataContext', '*CriteriaText', 'FieldExpressions', 'PackageName']);

  RegisterPropertiesInCategory(sDatabaseCategoryName, TdxEMFCustomQueryDataSet,
    [{'SkipSelectedRecords', 'TopSelectedRecords',} 'GroupByExpressionDefinitions', 'SortByExpressionDefinitions']);

  RegisterPropertiesInCategory(sLinkageCategoryName, TdxEMFCustomDataSet,
    ['DataContext', 'PackageName']);

  RegisterPropertiesInCategory(sLinkageCategoryName, TdxEMFCustomDataContext,
    ['PackageName']);
end;

initialization
  GroupDescendentsWith(TdxEMFCustomDataSet, TControl);
  GroupDescendentsWith(TdxEMFSession, TControl);
  GroupDescendentsWith(TdxEMFDataContext, TControl);
  GroupDescendentsWith(TdxEMFCustomDataProvider, TControl);
end.
